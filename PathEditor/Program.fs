open System
open Microsoft.Win32
open System.Runtime.InteropServices

module PathEditor =
    type Path =
        { Id : int
          Path : string
          Removed : Boolean
          Selected : Boolean
          AdminOnly : Boolean}
          

    let dir_removed d =
        d |> System.IO.Directory.Exists |> not
        

    let recount paths =
        paths
           |> Array.mapi<Path, Path> (fun i p -> { p with Id = i; Removed = dir_removed p.Path; Selected = false })


    let getpaths () =
        let machine = 
            Environment.GetEnvironmentVariable("Path", EnvironmentVariableTarget.Machine).ToString().Split(';')
                |> Array.mapi<string, Path> (fun i x -> { Id = i; Path = x; Removed = dir_removed x; Selected = false; AdminOnly = true })

        let users = 
            Environment.GetEnvironmentVariable("Path", EnvironmentVariableTarget.User).ToString().Split(';')
                |> Array.mapi<string, Path> (fun i x -> { Id = i; Path = x; Removed = dir_removed x; Selected = false; AdminOnly = false })
        
        Array.append machine users
            //|> Array.filter (fun p -> not p.AdminOnly) //for now :)
            |> recount


    let chosen_path paths =
        paths |> Array.findIndex (fun p -> p.Selected)


    

    let s_join (d: string) (xstr: string[]) =
        String.Join(d, xstr)

    let add (paths_arg : Path[])  =
        Console.Clear()
        printf "Enter new path: "
        let newpath = Console.ReadLine()

        printf "Store (g)lobally or for (u)ser: "
        let for_admin = 
            match Console.ReadLine() with
            | "g" -> true
            | _ -> false

        match Array.exists (fun p -> p.Path = newpath) paths_arg with
            | false -> Array.append paths_arg [|{ Id = paths_arg.Length; Path = newpath; Removed = dir_removed newpath; Selected = false; AdminOnly = for_admin }|]
            | true -> paths_arg


    let select_first paths = 
        paths |> Array.map (fun p -> if p.Id = 0 then { p with Selected = true } else p)


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
            paths_arg 
                |> Array.filter (fun p -> not p.AdminOnly)
                |> Array.map (fun p -> p.Path)
                |> s_join ";"

        printfn "%s\n" pathstr
        printf "The string above will be written to your %%PATH%%. Continue? (y/N): "
        let choice = Console.ReadLine()

        if choice = "y" || choice = "Y" then
            Environment.SetEnvironmentVariable("Path", pathstr, EnvironmentVariableTarget.User)
            printfn "Changes written!"
        else
            printfn "No changes made."
        
        printf "Press enter to continue.."
        Console.ReadLine() |> ignore     
        
        getpaths ()
            |> select_first


    let print_help() =
        Console.Clear()
        let help_msg = "[ PathEditor Help ]\n\
            \n\
            >\tdenotes the selected path\n\
            *\tdenotes that a path is not accessible on the disk\n\
            a\tadds a new path to the PATH environment variable\n\
            r\tremoves the selected path from the PATH environment variable\n\
            l\treloads registry\n\
            s\tsaves changes to registry\n\
            h\tprints this help\n\
            x\tcloses the program, discarding unsaved changes\n\
            \n\
            Press enter to continue.."

        printf "%s" help_msg

        Console.ReadLine() |> ignore     

    let arrow_up paths_arg =
        let selected =
            match (paths_arg |> chosen_path) with
                | 0 -> paths_arg.Length
                | _ -> paths_arg |> chosen_path

        paths_arg
            |> Array.map (fun p ->
                match (selected - 1) % (paths_arg.Length) = p.Id with
                    | true -> { p with Selected = true }
                    | false -> { p with Selected = false })


    let arrow_down paths_arg =
        paths_arg
            |> Array.map (fun p ->                
                match ((paths_arg |> chosen_path) + 1) % (paths_arg.Length) = p.Id with
                | true -> { p with Selected = true }
                | false -> { p with Selected = false })


    let print_paths paths =
        Console.Clear();
        let selected = paths |> chosen_path
        let min' = (selected - 5, 0) |> Math.Max
        let max' = (selected + 5, paths.Length - 1) |> Math.Min

        let min, max =
            match  max' - min' with
            | diff when diff < 10 && min' = 0 -> min', max' + (10 - diff)
            | diff when diff < 10 && max' = paths.Length - 1 ->  min' - (10 - diff), max'
            | _ -> min', max'

        match selected with
                | i when i > 5 -> printfn " ^ "
                | _ -> printfn " - "

        for path in (paths |> Array.filter (fun (p: Path) -> p.Id >= min && p.Id <= max)) do
            match path.Selected with
                | true -> printf "> "
                | _ -> ()

            match path.Removed with
                | true -> printfn "%d: * %s" path.Id path.Path
                | _ -> printfn "%d: %s" path.Id path.Path


        match selected with
            | i when i < paths.Length - 5 -> printfn " v "
            | _ -> printfn " - "


    let rec loop paths_arg =
        print_paths paths_arg

        printfn ""
        printfn "a) Add"
        printfn "r) Remove"
        //printfn "e) Edit"
        printfn "l) Load"
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
                | ConsoleKey.L -> getpaths ()  |> select_first
                | ConsoleKey.S -> save paths_arg
                | ConsoleKey.UpArrow -> arrow_up paths_arg
                | ConsoleKey.DownArrow -> arrow_down paths_arg
                | _ -> paths_arg

        if paths = paths_arg then
            match choice.Key with
                | ConsoleKey.Q
                | ConsoleKey.X -> exit 0
                | ConsoleKey.H -> print_help ()
                | _ -> printfn "Unrecognized option."

        loop paths


    [<EntryPoint>]
    let main argv =
        getpaths () |> select_first |> loop
        0 // return an integer exit code
