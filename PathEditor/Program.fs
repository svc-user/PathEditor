// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// Microsoft.Win32.Registry.GetValue(@"HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\",  "Path", null)

open System
open Microsoft.Win32

module PathEditor =
    let key_name = @"HKEY_USERS\S-1-5-21-2195505661-2592938423-597485938-1142\Environment"
    type Path =
        { Id : int
          Path : string
          Removed : Boolean
          Selected : Boolean}

    let dir_removed d =
        System.IO.Directory.Exists(d) |> not


    let getpaths =
        Registry.GetValue(key_name,  "Path", "").ToString().Split(';')
            |> Array.mapi<string, Path> (fun i x -> { Id = i; Path = x; Removed = dir_removed x; Selected = false })


    let chosen_path paths =
        paths |> Array.findIndex (fun p -> p.Selected)


    let recount paths =
        paths
           |> Array.mapi<Path, Path> (fun i p -> { Id = i; Path = p.Path; Removed = dir_removed p.Path; Selected = false })


    let add (paths_arg : Path[]) =
        printf "Enter new path: "
        let newpath = Console.ReadLine()

        if Array.exists (fun (p : Path) -> p.Path = newpath) paths_arg = false then
            Array.append paths_arg [|{ Id = paths_arg.Length; Path = newpath; Removed = dir_removed newpath; Selected = false }|]
        else
            paths_arg


    let edit paths_arg =
        printf "What number to edit: "
        let mutable toEdit = -1
        let success = Int32.TryParse(Console.ReadLine(), &toEdit)

        printf "Edit path: "
        let newpath = Console.ReadLine()

        let paths: Path[] =
            if Array.exists (fun (p : Path) -> p.Path = newpath) paths_arg = false then
                Array.append paths_arg [|{ Id = paths_arg.Length; Path = newpath; Removed = dir_removed newpath; Selected = false }|]
            else
                paths_arg
        paths


    let remove paths_arg =
        let toRem = paths_arg |> chosen_path

        Console.Clear()
        printf "Remove %s? (y/N): " paths_arg.[toRem].Path
        let choice = Console.ReadLine()

        if choice = "y" || choice = "Y" then
            if toRem < paths_arg.Length && toRem > -1 then
                Array.append paths_arg.[0 .. toRem - 1] paths_arg.[toRem + 1.. ]
            else
                paths_arg
            |> recount
            |> Array.map (fun p -> if p.Id = ((paths_arg.Length - 2, ((toRem, 0) |> Math.Max)) |> Math.Min) then { p with Selected = true } else p)
        else
            paths_arg



    let save (paths_arg: Path[]) =
        Console.Clear()
        let pathstr =
            (";", paths_arg |> Array.map (fun p -> p.Path))
            |> String.Join

        printfn "%s" pathstr
        printf "The string above will be written to your %%PATH%%. Continue? (y/N): "
        let choice = Console.ReadLine()

        if choice = "y" || choice = "Y" then
            Registry.SetValue(key_name, "Path", pathstr, RegistryValueKind.ExpandString)
            printfn "Changes written!"
        else
            printfn "No changes made."

        System.Threading.Thread.Sleep(1000)
        getpaths |> Array.map (fun p -> p)


    let print_help =
        printfn ""


    let up paths_arg =
        let index =
            paths_arg
                |> Array.findIndex (fun p -> p.Selected)

        let selected =
            match index with
                | 0 -> paths_arg.Length
                | _ -> index

        paths_arg
            |> Array.map (fun p ->
                if (selected - 1) % (paths_arg.Length) = p.Id then
                    { p with Selected = true }
                else
                    { p with Selected = false })


    let down paths_arg =
        let selected =
            paths_arg
                |> Array.findIndex (fun p -> p.Selected)

        paths_arg
            |> Array.map (fun p ->
                if (selected + 1) % (paths_arg.Length) = p.Id then
                    { p with Selected = true }
                else
                    { p with Selected = false })


    let rec loop paths_arg =
        Console.Clear();
        for path in paths_arg do
            if path.Selected then
                printf "> "
            if path.Removed then
                printfn "%d: * %s" path.Id path.Path
            else
                printfn "%d: %s" path.Id path.Path

        printfn ""
        printfn "a) Add"
        printfn "r) Remove"
        //printfn "e) Edit"
        printfn "s) Save"
        printfn "h) Help"
        printfn "x) Exit (discard non-saved changes)"
        printf " > "

        let choice = Console.ReadKey()
        let paths =
            match choice.Key with
                | ConsoleKey.A -> add paths_arg
                //| "e" -> edit paths_arg
                | ConsoleKey.R -> remove paths_arg
                | ConsoleKey.S -> save paths_arg
                | ConsoleKey.UpArrow -> up paths_arg
                | ConsoleKey.DownArrow -> down paths_arg
                | _ -> paths_arg

        if paths = paths_arg then
            match choice.Key with
                | ConsoleKey.X -> exit 0
                | ConsoleKey.P -> print_help
                | _ -> printfn "Unrecognized option."

        loop paths


    [<EntryPoint>]
    let main argv =
        getpaths |> Array.map (fun p -> if p.Id = 0 then { p with Selected = true } else p) |> loop
        0 // return an integer exit code
