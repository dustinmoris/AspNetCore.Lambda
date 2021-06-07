namespace Giraffe

[<AutoOpen>]
module Core =
    open System.Text
    open System.Threading.Tasks
    open System.Globalization
    open Microsoft.AspNetCore.Http
    open Microsoft.Extensions.Logging
    open FSharp.Control.Tasks
    open Giraffe.ViewEngine

    /// <summary>
    /// A type alias for <see cref="System.Threading.Tasks.Task{HttpContext option}" />  which represents the result of a HTTP function (HttpFunc).
    /// If the result is Some HttpContext then the Giraffe middleware will return the response to the client and end the pipeline. However, if the result is None then the Giraffe middleware will continue the ASP.NET Core pipeline by invoking the next middleware.
    /// </summary>
    type HttpFuncResult = Task<HttpContext option>

    /// <summary>
    /// A HTTP function which takes an <see cref="Microsoft.AspNetCore.Http.HttpContext"/> object and returns a <see cref="HttpFuncResult"/>.
    /// The function may inspect the incoming <see cref="Microsoft.AspNetCore.Http.HttpRequest"/> and make modifications to the <see cref="Microsoft.AspNetCore.Http.HttpResponse"/> before returning a <see cref="HttpFuncResult"/>. The result can be either a <see cref="System.Threading.Tasks.Task"/> of Some HttpContext or a <see cref="System.Threading.Tasks.Task"/> of None.
    /// If the result is Some HttpContext then the Giraffe middleware will return the response to the client and end the pipeline. However, if the result is None then the Giraffe middleware will continue the ASP.NET Core pipeline by invoking the next middleware.
    /// </summary>
    type HttpFunc = HttpContext -> HttpFuncResult

    /// <summary>
    /// A HTTP handler is the core building block of a Giraffe web application. It works similarly to ASP.NET Core's middleware where it is self responsible for invoking the next <see cref="HttpFunc"/> function of the pipeline or shortcircuit the execution by directly returning a <see cref="System.Threading.Tasks.Task"/> of HttpContext option.
    /// </summary>
    type HttpHandler = HttpFunc -> HttpFunc

    /// <summary>
    /// The error handler function takes an <see cref="System.Exception"/> object as well as an <see cref="Microsoft.Extensions.Logging.ILogger"/> instance and returns a <see cref="HttpHandler"/> function which takes care of handling any uncaught application errors.
    /// </summary>
    type ErrorHandler = exn -> ILogger -> HttpHandler

    // ---------------------------
    // Globally useful functions
    // ---------------------------

    /// <summary>
    /// The warbler function is a <see cref="HttpHandler"/> wrapper function which prevents a <see cref="HttpHandler"/> to be pre-evaluated at startup.
    /// </summary>
    /// <param name="f">A function which takes a HttpFunc * HttpContext tuple and returns a <see cref="HttpHandler"/> function.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <example>
    /// <code>
    /// warbler(fun _ -> someHttpHandler)
    /// </code>
    /// </example>
    /// <returns>Returns a <see cref="HttpHandler"/> function.</returns>
    let inline warbler f (next : HttpFunc) (ctx : HttpContext) = f (next, ctx) next ctx

    /// <summary>
    /// Use skipPipeline to shortcircuit the <see cref="HttpHandler"/> pipeline and return None to the surrounding <see cref="HttpHandler"/> or the Giraffe middleware (which would subsequently invoke the next middleware as a result of it).
    /// </summary>
    let skipPipeline : HttpFuncResult = Task.FromResult None

    /// <summary>
    /// Use earlyReturn to shortcircuit the <see cref="HttpHandler"/> pipeline and return Some HttpContext to the surrounding <see cref="HttpHandler"/> or the Giraffe middleware (which would subsequently end the pipeline by returning the response back to the client).
    /// </summary>
    let earlyReturn : HttpFunc = Some >> Task.FromResult

    // ---------------------------
    // Convenience Handlers
    // ---------------------------

    /// <summary>
    /// The handleContext function is a convenience function which can be used to create a new <see cref="HttpHandler"/> function which only requires access to the <see cref="Microsoft.AspNetCore.Http.HttpContext"/> object.
    /// </summary>
    /// <param name="contextMap">A function which accepts a <see cref="Microsoft.AspNetCore.Http.HttpContext"/> object and returns a <see cref="HttpFuncResult"/> function.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let handleContext (contextMap : HttpContext -> HttpFuncResult) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                match! contextMap ctx with
                | Some c ->
                    match c.Response.HasStarted with
                    | true  -> return  Some c
                    | false -> return! next c
                | None      -> return  None
            }

    // ---------------------------
    // Default Combinators
    // ---------------------------

    /// <summary>
    /// Combines two <see cref="HttpHandler"/> functions into one.
    /// Please mind that both <see cref="HttpHandler"/>  functions will get pre-evaluated at runtime by applying the next <see cref="HttpFunc"/> parameter of each handler.
    /// You can also use the fish operator `>=>` as a more convenient alternative to compose.
    /// </summary>
    /// <param name="handler1"></param>
    /// <param name="handler2"></param>
    /// <param name="final"></param>
    /// <returns>A <see cref="HttpFunc"/>.</returns>
    let compose (handler1 : HttpHandler) (handler2 : HttpHandler) : HttpHandler =
        fun (final : HttpFunc) ->
            let func = final |> handler2 |> handler1
            fun (ctx : HttpContext) ->
                match ctx.Response.HasStarted with
                | true  -> final ctx
                | false -> func ctx

    /// <summary>
    /// Combines two <see cref="HttpHandler"/> functions into one.
    /// Please mind that both <see cref="HttpHandler"/> functions will get pre-evaluated at runtime by applying the next <see cref="HttpFunc"/> parameter of each handler.
    /// </summary>
    let (>=>) = compose

    /// <summary>
    /// Iterates through a list of `HttpFunc` functions and returns the result of the first `HttpFunc` of which the outcome is `Some HttpContext`.
    /// </summary>
    /// <param name="funcs"></param>
    /// <param name="ctx"></param>
    /// <returns>A <see cref="HttpFuncResult"/>.</returns>
    let rec private chooseHttpFunc (funcs : HttpFunc list) : HttpFunc =
        fun (ctx : HttpContext) ->
            task {
                match funcs with
                | [] -> return None
                | func :: tail ->
                    let! result = func ctx
                    match result with
                    | Some c -> return Some c
                    | None   -> return! chooseHttpFunc tail ctx
            }

    /// <summary>
    /// Iterates through a list of <see cref="HttpHandler"/> functions and returns the result of the first <see cref="HttpHandler"/> of which the outcome is Some HttpContext.
    /// Please mind that all <see cref="HttpHandler"/> functions will get pre-evaluated at runtime by applying the next (HttpFunc) parameter to each handler.
    /// </summary>
    /// <param name="handlers"></param>
    /// <param name="next"></param>
    /// <returns>A <see cref="HttpFunc"/>.</returns>
    let choose (handlers : HttpHandler list) : HttpHandler =
        fun (next : HttpFunc) ->
            let funcs = handlers |> List.map (fun h -> h next)
            fun (ctx : HttpContext) ->
                chooseHttpFunc funcs ctx

    // ---------------------------
    // Default HttpHandlers
    // ---------------------------

    /// <summary>
    /// Filters an incoming HTTP request based on the HTTP verb.
    /// </summary>
    /// <param name="validate">A validation function which checks for a single HTTP verb.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let private httpVerb (validate : string -> bool) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            if validate ctx.Request.Method
            then next ctx
            else skipPipeline

    let GET     : HttpHandler = httpVerb HttpMethods.IsGet
    let POST    : HttpHandler = httpVerb HttpMethods.IsPost
    let PUT     : HttpHandler = httpVerb HttpMethods.IsPut
    let PATCH   : HttpHandler = httpVerb HttpMethods.IsPatch
    let DELETE  : HttpHandler = httpVerb HttpMethods.IsDelete
    let HEAD    : HttpHandler = httpVerb HttpMethods.IsHead
    let OPTIONS : HttpHandler = httpVerb HttpMethods.IsOptions
    let TRACE   : HttpHandler = httpVerb HttpMethods.IsTrace
    let CONNECT : HttpHandler = httpVerb HttpMethods.IsConnect

    let GET_HEAD : HttpHandler = choose [ GET; HEAD ]

    /// <summary>
    /// Clears the current <see cref="Microsoft.AspNetCore.Http.HttpResponse"/> object.
    /// This can be useful if a <see cref="HttpHandler"/> function needs to overwrite the response of all previous <see cref="HttpHandler"/> functions with its own response (most commonly used by an <see cref="ErrorHandler"/> function).
    /// </summary>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let clearResponse : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            ctx.Response.Clear()
            next ctx

    /// <summary>
    /// Sets the Content-Type HTTP header in the response.
    /// </summary>
    /// <param name="contentType">The mime type of the response (e.g.: application/json or text/html).</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let setContentType contentType : HttpHandler =
        fun next ctx ->
            ctx.SetContentType contentType
            next ctx

    /// <summary>
    /// Sets the HTTP status code of the response.
    /// </summary>
    /// <param name="statusCode">The status code to be set in the response. For convenience you can use the static <see cref="Microsoft.AspNetCore.Http.StatusCodes"/> class for passing in named status codes instead of using pure int values.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let setStatusCode (statusCode : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            ctx.SetStatusCode statusCode
            next ctx

    /// <summary>
    /// Adds or sets a HTTP header in the response.
    /// </summary>
    /// <param name="key">The HTTP header name. For convenience you can use the static <see cref="Microsoft.Net.Http.Headers.HeaderNames"/> class for passing in strongly typed header names instead of using pure string values.</param>
    /// <param name="value">The value to be set. Non string values will be converted to a string using the object's ToString() method.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let setHttpHeader (key : string) (value : obj) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            ctx.SetHttpHeader(key, value)
            next ctx

    /// <summary>
    /// Filters an incoming HTTP request based on the accepted mime types of the client (Accept HTTP header).
    /// If the client doesn't accept any of the provided mimeTypes then the handler will not continue executing the next <see cref="HttpHandler"/> function.
    /// </summary>
    /// <param name="mimeTypes">List of mime types of which the client has to accept at least one.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let mustAccept (mimeTypes : string list) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            let headers = ctx.Request.GetTypedHeaders()
            headers.Accept
            |> Seq.map    (fun h -> h.ToString())
            |> Seq.exists (fun h -> mimeTypes |> Seq.contains h)
            |> function
                | true  -> next ctx
                | false -> skipPipeline

    /// <summary>
    /// Redirects to a different location with a `302` or `301` (when permanent) HTTP status code.
    /// </summary>
    /// <param name="permanent">If true the redirect is permanent (301), otherwise temporary (302).</param>
    /// <param name="location">The URL to redirect the client to.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let redirectTo (permanent : bool) (location : string) : HttpHandler  =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            ctx.Response.Redirect(location, permanent)
            Task.FromResult (Some ctx)

    // ---------------------------
    // Model binding functions
    // ---------------------------

    /// <summary>
    /// Parses a JSON payload into an instance of type 'T.
    /// </summary>
    /// <param name="f">A function which accepts an object of type 'T and returns a <see cref="HttpHandler"/> function.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <typeparam name="'T"></typeparam>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let bindJson<'T> (f : 'T -> HttpHandler) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! model = ctx.BindJsonAsync<'T>()
                return! f model next ctx
            }

    /// <summary>
    /// Parses a XML payload into an instance of type 'T.
    /// </summary>
    /// <param name="f">A function which accepts an object of type 'T and returns a <see cref="HttpHandler"/> function.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <typeparam name="'T"></typeparam>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let bindXml<'T> (f : 'T -> HttpHandler) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! model = ctx.BindXmlAsync<'T>()
                return! f model next ctx
            }

    /// <summary>
    /// Parses a HTTP form payload into an instance of type 'T.
    /// </summary>
    /// <param name="culture">An optional <see cref="System.Globalization.CultureInfo"/> element to be used when parsing culture specific data such as float, DateTime or decimal values.</param>
    /// <param name="f">A function which accepts an object of type 'T and returns a <see cref="HttpHandler"/> function.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <typeparam name="'T"></typeparam>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let bindForm<'T> (culture : CultureInfo option) (f : 'T -> HttpHandler) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! model =
                    match culture with
                    | Some c -> ctx.BindFormAsync<'T> c
                    | None   -> ctx.BindFormAsync<'T>()
                return! f model next ctx
            }

    /// <summary>
    /// Tries to parse a HTTP form payload into an instance of type 'T.
    /// </summary>
    /// <param name="parsingErrorHandler">A <see cref="System.String"/> -> <see cref="HttpHandler"/> function which will get invoked when the model parsing fails. The <see cref="System.String"/> parameter holds the parsing error message.</param>
    /// <param name="culture">An optional <see cref="System.Globalization.CultureInfo"/> element to be used when parsing culture specific data such as float, DateTime or decimal values.</param>
    /// <param name="successHandler">A function which accepts an object of type 'T and returns a <see cref="HttpHandler"/> function.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <typeparam name="'T"></typeparam>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let tryBindForm<'T> (parsingErrorHandler : string -> HttpHandler)
                        (culture             : CultureInfo option)
                        (successHandler      : 'T -> HttpHandler) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! result =
                    match culture with
                    | Some c -> ctx.TryBindFormAsync<'T> c
                    | None   -> ctx.TryBindFormAsync<'T>()
                return!
                    (match result with
                    | Error msg -> parsingErrorHandler msg
                    | Ok model  -> successHandler model) next ctx
            }

    /// <summary>
    /// Parses a HTTP query string into an instance of type 'T.
    /// </summary>
    /// <param name="culture">An optional <see cref="System.Globalization.CultureInfo"/> element to be used when parsing culture specific data such as float, DateTime or decimal values.</param>
    /// <param name="f">A function which accepts an object of type 'T and returns a <see cref="HttpHandler"/> function.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <typeparam name="'T"></typeparam>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let bindQuery<'T> (culture : CultureInfo option) (f : 'T -> HttpHandler) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            let model =
                match culture with
                | Some c -> ctx.BindQueryString<'T> c
                | None   -> ctx.BindQueryString<'T>()
            f model next ctx

    /// <summary>
    /// Tries to parse a query string into an instance of type `'T`.
    /// </summary>
    /// <param name="parsingErrorHandler">A <see href="HttpHandler"/> function which will get invoked when the model parsing fails. The <see cref="System.String"/> input parameter holds the parsing error message.</param>
    /// <param name="culture">An optional <see cref="System.Globalization.CultureInfo"/> element to be used when parsing culture specific data such as float, DateTime or decimal values.</param>
    /// <param name="successHandler">A function which accepts an object of type 'T and returns a <see cref="HttpHandler"/> function.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <typeparam name="'T"></typeparam>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let tryBindQuery<'T> (parsingErrorHandler : string -> HttpHandler)
                         (culture             : CultureInfo option)
                         (successHandler      : 'T -> HttpHandler) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            let result =
                match culture with
                | Some c -> ctx.TryBindQueryString<'T> c
                | None   -> ctx.TryBindQueryString<'T>()
            (match result with
            | Error msg -> parsingErrorHandler msg
            | Ok model  -> successHandler model) next ctx

    /// <summary>
    /// Parses a HTTP payload into an instance of type 'T.
    /// The model can be sent via XML, JSON, form or query string.
    /// </summary>
    /// <param name="culture">An optional <see cref="System.Globalization.CultureInfo"/> element to be used when parsing culture specific data such as float, DateTime or decimal values.</param>
    /// <param name="f">A function which accepts an object of type 'T and returns a <see cref="HttpHandler"/> function.</param>
    /// <param name="next"></param>
    /// <param name="ctx"></param>
    /// <typeparam name="'T"></typeparam>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let bindModel<'T> (culture : CultureInfo option) (f : 'T -> HttpHandler) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! model =
                    match culture with
                    | Some c -> ctx.BindModelAsync<'T> c
                    | None   -> ctx.BindModelAsync<'T>()
                return! f model next ctx
            }

    // ---------------------------
    // Response writing functions
    // ---------------------------

    /// **Description**
    ///
    /// Writes a byte array to the body of the HTTP response and sets the HTTP `Content-Length` header accordingly.
    ///
    /// **Parameters**
    ///
    /// `bytes`: The byte array to be send back to the client.
    ///
    /// **Output**
    ///
    /// A Giraffe <see cref="HttpHandler" /> function which can be composed into a bigger web application.

    /// <summary>
    /// Writes a byte array to the body of the HTTP response and sets the HTTP Content-Length header accordingly.
    /// </summary>
    /// <param name="bytes">The byte array to be send back to the client.</param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler" /> function which can be composed into a bigger web application.</returns>
    let setBody (bytes : byte array) : HttpHandler =
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            ctx.WriteBytesAsync bytes

    /// <summary>
    /// Writes an UTF-8 encoded string to the body of the HTTP response and sets the HTTP Content-Length header accordingly.
    /// </summary>
    /// <param name="str">The string value to be send back to the client.</param>
    /// <returns>A Giraffe <see cref="HttpHandler" /> function which can be composed into a bigger web application.</returns>
    let setBodyFromString (str : string) : HttpHandler =
        let bytes = Encoding.UTF8.GetBytes str
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            ctx.WriteBytesAsync bytes

    /// <summary>
    /// Writes an UTF-8 encoded string to the body of the HTTP response and sets the HTTP Content-Length header accordingly, as well as the Content-Type header to text/plain.
    /// </summary>
    /// <param name="str">The string value to be send back to the client.</param>
    /// <returns>A Giraffe <see cref="HttpHandler" /> function which can be composed into a bigger web application.</returns>
    let text (str : string) : HttpHandler =
        let bytes = Encoding.UTF8.GetBytes str
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            ctx.SetContentType "text/plain; charset=utf-8"
            ctx.WriteBytesAsync bytes

    /// <summary>
    /// Serializes an object to JSON and writes the output to the body of the HTTP response.
    /// It also sets the HTTP Content-Type header to application/json and sets the Content-Length header accordingly.
    /// The JSON serializer can be configured in the ASP.NET Core startup code by registering a custom class of type <see cref="Json.ISerializer"/>.
    /// </summary>
    /// <param name="dataObj">The object to be send back to the client.</param>
    /// <param name="ctx"></param>
    /// <typeparam name="'T"></typeparam>
    /// <returns>A Giraffe <see cref="HttpHandler" /> function which can be composed into a bigger web application.</returns>
    let json<'T> (dataObj : 'T) : HttpHandler =
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            ctx.WriteJsonAsync dataObj

    /// <summary>
    /// Serializes an object to JSON and writes the output to the body of the HTTP response using chunked transfer encoding.
    /// It also sets the HTTP Content-Type header to application/json and sets the Transfer-Encoding header to chunked.
    /// The JSON serializer can be configured in the ASP.NET Core startup code by registering a custom class of type <see cref="Json.ISerializer"/>.
    /// </summary>
    /// <param name="dataObj">The object to be send back to the client.</param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler" /> function which can be composed into a bigger web application.</returns>
    let jsonChunked<'T> (dataObj : 'T) : HttpHandler =
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            ctx.WriteJsonChunkedAsync dataObj

    /// <summary>
    /// Serializes an object to XML and writes the output to the body of the HTTP response.
    /// It also sets the HTTP Content-Type header to application/xml and sets the Content-Length header accordingly.
    /// The JSON serializer can be configured in the ASP.NET Core startup code by registering a custom class of type <see cref="Xml.ISerializer"/>.
    /// </summary>
    /// <param name="dataObj">The object to be send back to the client.</param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler" /> function which can be composed into a bigger web application.</returns>
    let xml (dataObj : obj) : HttpHandler =
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            ctx.WriteXmlAsync dataObj

    /// <summary>
    /// Reads a HTML file from disk and writes its contents to the body of the HTTP response.
    /// It also sets the HTTP header Content-Type to text/html and sets the Content-Length header accordingly.
    /// </summary>
    /// <param name="filePath">A relative or absolute file path to the HTML file.</param>
    /// <param name="ctx"></param>
    /// <returns>A Giraffe <see cref="HttpHandler" /> function which can be composed into a bigger web application.</returns>
    let htmlFile (filePath : string) : HttpHandler =
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            ctx.WriteHtmlFileAsync filePath

    /// <summary>
    /// Writes a HTML string to the body of the HTTP response.
    /// It also sets the HTTP header Content-Type to text/html and sets the Content-Length header accordingly.
    /// </summary>
    /// <param name="html">The HTML string to be send back to the client.</param>
    /// <returns>A Giraffe <see cref="HttpHandler" /> function which can be composed into a bigger web application.</returns>
    let htmlString (html : string) : HttpHandler =
        let bytes = Encoding.UTF8.GetBytes html
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            ctx.SetContentType "text/html; charset=utf-8"
            ctx.WriteBytesAsync bytes

    /// <summary>
    /// <para>Compiles a `Giraffe.GiraffeViewEngine.XmlNode` object to a HTML view and writes the output to the body of the HTTP response.</para>
    /// <para>It also sets the HTTP header `Content-Type` to `text/html` and sets the `Content-Length` header accordingly.</para>
    /// </summary>
    /// <param name="htmlView">An `XmlNode` object to be send back to the client and which represents a valid HTML view.</param>
    /// <returns>A Giraffe `HttpHandler` function which can be composed into a bigger web application.</returns>
    let htmlView (htmlView : XmlNode) : HttpHandler =
        let bytes = RenderView.AsBytes.htmlDocument htmlView
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            ctx.SetContentType "text/html; charset=utf-8"
            ctx.WriteBytesAsync bytes