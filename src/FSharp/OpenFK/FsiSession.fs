namespace OpenFK

open System.Diagnostics

type FsiSession (fsiPath: string) =

    let info = new ProcessStartInfo ()
    let fsiProcess = new Process ()

    do
        info.RedirectStandardInput <- true
        info.RedirectStandardOutput <- true
        info.UseShellExecute <- false
        info.CreateNoWindow <- true
        info.FileName <- fsiPath

        fsiProcess.StartInfo <- info

    [<CLIEvent>]
    member this.OutputReceived = fsiProcess.OutputDataReceived

    [<CLIEvent>]
    member this.ErrorReceived = fsiProcess.ErrorDataReceived

    member this.Start () =
        fsiProcess.Start ()
        fsiProcess.BeginOutputReadLine ()

    member this.AddLine (line: string) =
        fsiProcess.StandardInput.WriteLine(line)

    member this.Evaluate () =
        this.AddLine (";;")
        fsiProcess.StandardInput.Flush()

