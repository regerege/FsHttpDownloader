open System
open System.Net
open System.Net.Security
open System.Net.Sockets
open System.IO
open System.Text

///<summary>送信文字列をASCIIのバイナリデータに変換する。</summary>
///<param name="post">POSTデータ</param>
let getData (post:string) =
    Encoding.ASCII.GetBytes(post)
    |> (fun x -> (x, int64 <| x.Length))

let combineFile file list =
    use fs = File.Create(file)
    list
    |> Seq.sortBy fst
    |> Seq.iter(fun (_,path:string) ->
        use fs2 = File.OpenRead path
        seq { while true do yield fs2.ReadByte() }
        |> Seq.takeWhile ((<=)0)
        |> Seq.iter(fun n -> fs.WriteByte(byte n)))

/// 指定URLに対して文字列のデータをASCIIバイナリで送信を行い、
/// HTMLをダウンロードする。
let wget (url:string) post (tc:int) =
    let root = Path.GetDirectoryName <| System.Reflection.Assembly.GetExecutingAssembly().Location
    let seg = (new Uri(url)).Segments |> Seq.fold (fun acm s -> s) ""
    let (name,ex) = let a = seg.Split('.') in a.[0],a.[1]
    let getRequest() =
        let req = WebRequest.Create(url) :?> HttpWebRequest
        // 送信
        let (data,len) = getData post
        if 0L < len then
            req.ContentLength <- len
            use reqs = req.GetRequestStream()
            reqs.Write(data, 0, data.Length)
        req
    let getContent() =
        use res = getRequest().GetResponse() :?> HttpWebResponse
        res.StatusCode,res.ContentLength
    let status,len = getContent();
    if status <> HttpStatusCode.PartialContent then
        let bs = len / int64 tc
        let residue = len % int64 tc
        let positions =
            let rec f l n =
                if len + 1L <= n then l
                else f (l@[n]) (n + bs + (if n = 0L then residue else 0L))
            f [] 0L
        Seq.init tc (fun id ->
            async {
                let path = Path.Combine(root, sprintf "[%d]%s.tmp" id name)
                let pos = positions.[id] + (if 0 < id then 1L else 0L)
                let nextpos = positions.[id + 1]
                let size = nextpos - pos
                let op = size / 100L
                let traceproc n =
                    if n % op = 0L then
                        printfn "%d : %d%%" <| id <| n / op

                let req = getRequest()
                req.AddRange(pos,nextpos)
                use res = req.GetResponse()
                use ress = res.GetResponseStream()
                use fs = File.Create(path)
                let write = byte >> fs.WriteByte
                seq { while true do yield ress.ReadByte() }
                |> Seq.takeWhile ((<=)0)
                |> Seq.scan (fun (c,_) d ->
                    traceproc c
                    (c+1L,d)) (0L,0)
                |> Seq.skip 1
                |> Seq.map snd
                |> Seq.iter write
                return (id,path)
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> combineFile (Path.Combine(root, seg))

wget "http://localhost/test/winxp.rar" "" 5
//#if COMPILED
//[<STAThread>]
//do
//    wget "http://localhost/test/win2012.rar" "" 5
//#endif
