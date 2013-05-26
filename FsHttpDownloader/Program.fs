open System
open System.Drawing
open System.Collections.ObjectModel
open System.ComponentModel
open System.Net
open System.Net.Security
open System.Net.Sockets
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Input
open System.IO
open System.Text
open System.Threading
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.Win32

let lock (lockobj:obj) f =
    Monitor.Enter lockobj
    try
        f()
    finally
        Monitor.Exit lockobj

///<summary>送信文字列をASCIIのバイナリデータに変換する。</summary>
///<param name="post">POSTデータ</param>
let getData (post:string) =
    Encoding.ASCII.GetBytes(post)
    |> (fun x -> (x, int64 <| x.Length))

/// 分割されたファイルをソートした後に結合する。
let combineFile file list =
    use fs = File.Create(file)
    list
    |> Seq.sortBy fst
    |> Seq.iter(fun (_,path:string) ->
        use fs2 = File.OpenRead path
        seq { while true do yield fs2.ReadByte() }
        |> Seq.takeWhile ((<=)0)
        |> Seq.iter(fun n -> fs.WriteByte(byte n)))

let getRequest (url:string) post =
    let req = WebRequest.Create(url) :?> HttpWebRequest
    // 送信
    let (data,len) = getData post
    if 0L < len then
        req.ContentLength <- len
        use reqs = req.GetRequestStream()
        reqs.Write(data, 0, data.Length)
    req
let getContent url post =
    use res = (getRequest url post).GetResponse() :?> HttpWebResponse
    res.StatusCode,res.ContentLength

let wget url post path (startpos:int64) endpos (pb:float -> unit) (isPause:unit -> bool) (op:int64) =
    let req = getRequest url post
    req.AddRange(startpos,endpos)
    use res = req.GetResponse()
    use ress = res.GetResponseStream()
    use fs = File.Create(path)
    let write = byte >> fs.WriteByte
    seq {
        while true do
            while isPause() do System.Threading.Thread.Sleep (250)
            yield ress.ReadByte() }
    |> Seq.takeWhile ((<=)0)
    |> Seq.scan (fun (c,_) d ->
        if c % op = 0L then
            pb <| (float c / float op)
        (c+1L,d)) (0L,0)
    |> Seq.skip 1
    |> Seq.map snd
    |> Seq.iter write
    path

/// 分割ダウンロードの開始
let startDownload url post (pbs:(float -> unit) list) (isPause:unit -> bool) (reset:unit -> unit) (path:string) (threadcount:int) =
    try
        let status,len = getContent url post;
        if status <> HttpStatusCode.PartialContent then
            let root = Path.GetDirectoryName path
            let filename = (new Uri(url)).Segments |> Seq.fold (fun acm s -> s) ""
            let name,ex = let a = filename.Split('.') in a.[0],a.[1]
            let temps = Array.init threadcount (fun id -> Path.Combine(root, sprintf "[%d]%s.tmp" id name))

            let bs = len / int64 threadcount
            let residue = len % int64 threadcount
            let positions =
                let rec f l n =
                    if len + 1L <= n then l
                    else f (l@[n]) (n + bs + (if n = 0L then residue else 0L))
                f [] 0L

            Seq.init threadcount (fun id ->
                let pos = positions.[id] + (if 0 < id then 1L else 0L)
                let nextpos = positions.[id + 1]
                let size = nextpos - pos
                let op = size / 100L
                async { let path = wget url post temps.[id] pos nextpos pbs.[id] isPause op in return id,path })
            |> Async.Parallel
            |> Async.RunSynchronously
            |> combineFile path
    with
    | ex -> MessageBox.Show (ex.Message, "例外エラー") |> ignore
    reset()

let asyncStartDownload token url post (pbs:(float -> unit) list) (isPause:unit -> bool) (reset:unit -> unit) (path:string) (threadcount:int) =
    let a = async { startDownload url post pbs isPause reset path threadcount }
    try
        Async.Start (a, token)
    with
    | ex -> MessageBox.Show (ex.Message, "例外エラー") |> ignore

/// ICommandオブジェクトを作成する。
let CreateCommand canExecute action =
    let event = Event<_,_>()
    { new System.Windows.Input.ICommand with
        member x.CanExecute obj = canExecute obj
        member x.Execute obj = action obj
        [<CLIEvent>]
        member x.CanExecuteChanged = event.Publish }

/// プロパティ変更後通知オブジェクト
type ViewModelBase() =
    let _propertyChanged = Event<_,_>()
    let toPropName(query : Expr) =
        match query with
        | PropertyGet(a, b, list) -> b.Name
        | _ -> ""

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = _propertyChanged.Publish

    abstract member OnPropertyChanged : string -> unit
    default x.OnPropertyChanged propertyName =
        _propertyChanged.Trigger(x, new PropertyChangedEventArgs(propertyName))

    member x.OnPropertyChanged(expr : Expr) =
        expr
        |> toPropName
        |> x.OnPropertyChanged

type FsHttpDownloaderWPF() as x =
    inherit ViewModelBase()

    let obj = new Object()
    let token = Async.DefaultCancellationToken

    let mutable _puase = false
    let mutable _started = false
    let mutable _url = "http://localhost/test/winxp.rar"
    let mutable _path = @"D:\winxp.rar"
    let mutable _threadcount = 1

    let _wpf = Application.LoadComponent(new Uri("FsHttpDownloaderWPF.xaml", System.UriKind.Relative)) :?> Window
    let _progressBarBorder = _wpf.FindName("progressBarBorder") :?> Border
    let _progressBarPanel = _wpf.FindName("progressBarPanel") :?> StackPanel
    let _progressBars =
        seq { 1..9 }
        |> Seq.map (sprintf "progressBar%d")
        |> Seq.map (fun s -> _wpf.FindName s :?> ProgressBar)
        |> Seq.toList
    let _changeProgress =
        _progressBars
        |> List.map(fun pb -> (fun p -> pb.Dispatcher.Invoke(new Action(fun () -> pb.Value <- p), null) |> ignore))
    let _btnStartPause = _wpf.FindName("btnStartPause") :?> Button

    let changeThreadCount tc =
        let width = _progressBarPanel.ActualWidth
        let w = width / (float tc)
        _progressBars
        |> Seq.iteri (fun i pb ->
            if i < tc then
                pb.Width <- w
                pb.Visibility <- Visibility.Visible
            else pb.Visibility <- Visibility.Collapsed)
    let isPause() = _puase
    let initFlag() =
        _puase <- false
        _started <- false
        _btnStartPause.Dispatcher.Invoke(new Action(fun _ -> _btnStartPause.Content <- "Start"), null) |> ignore

    do
        _wpf.DataContext <- x
        _wpf.Loaded.Add(fun _ -> changeThreadCount 1)

    member x.WPF = _wpf
    member x.ProgressBarPanel = _progressBarPanel

    member x.Url
        with get() = _url
        and set v =
            _url <- v
            base.OnPropertyChanged <@ x.Url @>
    member x.Path
        with get() = _path
        and set v =
            _path <- v
            base.OnPropertyChanged <@ x.Path @>
    member x.ThreadCount
        with get() = _threadcount
        and set v =
            if 1 <= v && v <= 9 then
                _threadcount <- v
                changeThreadCount v
                base.OnPropertyChanged <@ x.ThreadCount @>
    member x.ProgressBars with get() = _progressBars

    member x.OpenFile =
        CreateCommand (fun _ -> not _started)
            <| (fun _ ->
                let ofd = new SaveFileDialog()
                if ofd.ShowDialog().Value then
                    x.Path <- ofd.FileName)
    member x.Up =
        CreateCommand (fun _ -> not _started)
            <| (fun _ -> x.ThreadCount <- x.ThreadCount + 1)
    member x.Down =
        CreateCommand (fun _ -> not _started)
            <| (fun _ -> x.ThreadCount <- x.ThreadCount - 1)
    member x.Close =
        CreateCommand (fun _ -> true)
            <| (fun _ ->
                if _started then
                    MessageBox.Show ("分割ダウンロード中です。終了しますか？", "終了確認", MessageBoxButton.YesNo)
                    |> (fun (m:MessageBoxResult) ->
                        Async.CancelDefaultToken()
                        m = MessageBoxResult.Yes)
                else true
                |> function
                | true ->
                    Async.CancelDefaultToken()
                    _wpf.Close()
                | false -> ())
    member x.StartPuase =
        CreateCommand (fun _ -> true)
            <| (fun _ ->
                if _started then
                    _puase <- not _puase
                    _btnStartPause.Content <- if _puase then "Continue" else "Pause"
                else
                    _puase <- false
                    _started <- true
                    _btnStartPause.Content <- "Pause"

                    asyncStartDownload
                        <| token
                        <| _url
                        <| ""
                        <| _changeProgress
                        <| isPause
                        <| initFlag
                        <| _path
                        <| _threadcount)

[<STAThread>]
[<EntryPoint>]
let run _ =
    let downloader = new FsHttpDownloaderWPF()
    (new Application()).Run(downloader.WPF)

