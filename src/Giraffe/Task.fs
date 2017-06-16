[<AutoOpenAttribute>]
module Giraffe.Task
open System
open System.Collections
open System.Collections.Generic

open System.Threading
open System.Threading.Tasks
open System.Runtime.ExceptionServices

let inline wait (task:Task<_>) = task.Wait()

let inline delay (delay:TimeSpan) = 
   let tcs = TaskCompletionSource()
   Task.Delay(delay).ContinueWith(fun _ -> tcs.SetResult()) |> ignore
   tcs.Task

let toAsync (t: Task<'T>): Async<'T> =
   let abegin (cb: AsyncCallback, state: obj) : IAsyncResult = 
      match cb with
      | null -> upcast t
      | cb -> 
            t.ContinueWith(fun (_ : Task<_>) -> cb.Invoke t) |> ignore
            upcast t
   let aend (r: IAsyncResult) = 
      (r :?> Task<'T>).Result
   Async.FromBeginEnd(abegin, aend)

/// Transforms a Task's first value by using a specified mapping function.
let inline mapWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) f (m: Task<_>) =
   m.ContinueWith((fun (t: Task<_>) -> f t.Result), token, continuationOptions, scheduler)

/// Transforms a Task's first value by using a specified mapping function.
let inline map f (m: Task<_>) =
   m.ContinueWith(fun (t: Task<_>) -> f t.Result)


let inline bindTaskWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) (f: unit -> Task<'U>) (m: Task) =
   m.ContinueWith((fun _ -> f ()), token, continuationOptions, scheduler).Unwrap()

let inline bindWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) (f: 'T -> Task<'U>) (m: Task<'T>) =
   m.ContinueWith((fun (x: Task<_>) -> f x.Result), token, continuationOptions, scheduler).Unwrap()

let inline bind (f: 'T -> Task<'U>) (m: Task<'T>) = 
   m.ContinueWith(fun (x: Task<_>) -> f x.Result).Unwrap()

let inline returnM a = 
   let s = TaskCompletionSource()
   s.SetResult a
   s.Task

let inline whenAll f (tasks : Task<_> seq) = Task.WhenAll(tasks) |> map(f)

let inline private flip f a b = f b a

let inline private konst a _ = a
    
type TaskBuilder(?continuationOptions, ?scheduler, ?cancellationToken) =
   let contOptions = defaultArg continuationOptions TaskContinuationOptions.None
   let scheduler = defaultArg scheduler TaskScheduler.Default
   let cancellationToken = defaultArg cancellationToken CancellationToken.None

   member this.Return x = returnM x

   member this.Zero() = returnM ()

   member this.ReturnFrom (a: Task<'T>) = a

   member this.Bind(m:Task<'T>, f:'T->Task<'U>) = 
      if m.IsFaulted then
            let tcs = TaskCompletionSource<'U>()           
            let be = m.Exception.GetBaseException()
            raise be
            // tcs.SetException(be)
            // tcs.Task
      else      
            bindWithOptions cancellationToken contOptions scheduler f m

   member this.Combine(comp1, comp2) =
      this.Bind(comp1, comp2)

   member this.While(guard, m) =
      if not(guard()) then this.Zero() else
            this.Bind(m(), fun () -> this.While(guard, m))

   member this.TryWith(body:unit -> Task<'T>, catchFn:exn -> Task<'T>) =  
      try
         body()
          .ContinueWith(fun (t:Task<'T>) ->
             match t.IsFaulted with
             | false -> t
             | true  -> catchFn(t.Exception.GetBaseException()))
          .Unwrap()
      with e -> catchFn(e)

      
   member this.TryFinally(body:unit->Task<'T>, compensation) =
      let wrapOk (x:'a) : Task<'a> =
          compensation()
          this.Return x

      let wrapCrash (e:exn) : Task<'a> =
            printfn ">> the following exception has been receieved : %A" e.Message
            compensation()
            ExceptionDispatchInfo.Capture(e).Throw() 
            raise e
            // let tcs = TaskCompletionSource<_>()
            // tcs.SetException(e)
            // tcs.Task
      
      this.Bind(this.TryWith(body, wrapCrash), wrapOk)
   member this.Using(res: #IDisposable, body: #IDisposable -> Task<'T>) =
      this.TryFinally(
            (fun () -> body res),
            (fun () -> match res with null -> () | disp -> disp.Dispose())
            )

   member this.For(sequence: seq<_>, body) =
      this.Using(sequence.GetEnumerator(),
                     fun enum -> this.While(enum.MoveNext, fun () -> body enum.Current))

   member this.Delay (body : unit -> Task<'a>) : unit -> Task<'a> = fun () -> this.Bind(this.Return(), body)

   member this.Run (body: unit -> Task<'T>) = body()

   member this.AwaitTask (t:Task) =      
      let tcs = TaskCompletionSource<_>()
      let inline continuation (t:Task) : Task  = 
            match t.IsFaulted with
            | false ->  if t.IsCanceled 
                        then tcs.SetCanceled()
                        else tcs.SetResult()     
            | true  -> 
                  let be = t.Exception.GetBaseException()
                  tcs.SetException(be)
            t

      t.ContinueWith(
            continuation,
            cancellationToken,
            contOptions,
            scheduler) |> ignore
      tcs.Task

let task = TaskBuilder() //scheduler = TaskScheduler.Current
