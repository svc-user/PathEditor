// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// Microsoft.Win32.Registry.GetValue(@"HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\",  "Path", null)

open System;

module PathEditor =
    type Path =
        { Id: int
          Path: string }

    let getpaths = 
        Microsoft.Win32.Registry.GetValue(@"HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\",  "Path", "").ToString().Split(';')
            |> Array.mapi<string, Path> (fun i x -> { Id = i; Path = x})

    let recount paths =
        paths
           |> Array.mapi<Path, Path> (fun i p -> {Id = i; Path = p.Path})

    let add (pathsarg : Path[]) =
        printf "Enter new path: "
        let newpath = Console.ReadLine()

        let paths: Path[] =
            if Array.exists (fun (p : Path) -> p.Path = newpath) pathsarg = false then
                Array.append pathsarg [|{ Id = pathsarg.Length; Path = newpath }|]
            else
                pathsarg

        paths

    let edit paths =
        paths

    let remove (pathsarg: Path[]) =
        printf "What number to remove: "
        let mutable toRem = -1
        let success = Int32.TryParse(Console.ReadLine(), &toRem)

        let paths = 
            if toRem < pathsarg.Length && toRem > -1 && success then
                Array.append pathsarg.[0 .. toRem - 1] pathsarg.[toRem + 1.. ]
            else
                pathsarg
            |> recount

        paths

    let save (pathsarg: Path[]) = 
        Console.Clear()
        let pathstr = (String.Join ";", (pathsarg |> Array.map (fun (p: Path) -> p.Path)))
        printfn "%s" pathstr
        Console.ReadKey() |> ignore

        pathsarg

    let rec loop pathsArg = 

        Console.Clear();
        for path in pathsArg do
            printfn "%d: %s" path.Id path.Path

        printfn ""
        printfn "a) Add"
        printfn "r) Remove"
        printfn "e) Edit"
        printfn "s) Save"
        printfn "x) Exit (discard non-saved changes)"
        printf " > "
        
        let choice = Console.ReadLine()

        let paths =
            match choice with
                | "a" -> add pathsArg
                | "e" -> edit pathsArg
                | "r" -> remove pathsArg
                | "s" -> save pathsArg
                | _ -> pathsArg

        if paths = pathsArg then
            match choice with
                | "x" -> exit 0
                | _ -> printfn "Unrecognized option."

        loop paths
        
    [<EntryPoint>]
    let main argv = 
        loop getpaths |> ignore
        0 // return an integer exit code
