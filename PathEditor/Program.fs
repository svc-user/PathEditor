// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// Microsoft.Win32.Registry.GetValue(@"HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\",  "Path", null)

open System
open Microsoft.Win32
open System.Runtime.InteropServices

module PathEditor =
    let key_name = @"HKEY_USERS\S-1-5-21-2385905891-1455851501-2109537457-1001\Environment"
    type Path =
        { Id : int
          Path : string
          Removed : Boolean
          Selected : Boolean}

    let dir_removed d =
        d |> System.IO.Directory.Exists |> not


    let getpaths () =
        Registry.GetValue(key_name,  "Path", "").ToString().Split(';')
            |> Array.mapi<string, Path> (fun i x -> { Id = i; Path = x; Removed = dir_removed x; Selected = false })


    let chosen_path paths =
        paths |> Array.findIndex (fun p -> p.Selected)


    let recount paths =
        paths
           |> Array.mapi<Path, Path> (fun i p -> { Id = i; Path = p.Path; Removed = dir_removed p.Path; Selected = false })

    let s_join (d: string) (xstr: string[]) =
        String.Join(d, xstr)

    let add (paths_arg : Path[]) =
        Console.Clear()
        printf "Enter new path: "
        let newpath = Console.ReadLine()

        if Array.exists (fun (p : Path) -> p.Path = newpath) paths_arg = false then
            Array.append paths_arg [|{ Id = paths_arg.Length; Path = newpath; Removed = dir_removed newpath; Selected = false }|]
        else
            paths_arg

    let select_first paths = 
        paths |> Array.map (fun p -> if p.Id = 0 then { p with Selected = true } else p)
    (*
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
    *)

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
    
    [<DllImport("user32.dll", SetLastError=true, CharSet=CharSet.Auto)>]
    extern IntPtr SendMessage(
        IntPtr hWnd,
        uint32 Msg, 
        UIntPtr wParam,
        IntPtr lParam);

    let save (paths_arg: Path[]) =
        Console.Clear()
        let pathstr =
            paths_arg 
                |> Array.map (fun p -> p.Path)
                |> s_join ";"

        printfn "%s\n" pathstr
        printf "The string above will be written to your %%PATH%%. Continue? (y/N): "
        let choice = Console.ReadLine()

        if choice = "y" || choice = "Y" then
            Registry.SetValue(key_name, "Path", pathstr, RegistryValueKind.ExpandString)
            printfn "Changes written!"
        else
            printfn "No changes made."
        
        let WM_SETTINGCHANGE: uint32 = 0x001Au
        let HWND_BROADCAST: IntPtr = new IntPtr 0xffff
        SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, UIntPtr.Zero, IntPtr.Zero) |> ignore
        printf "Press enter to continue.."
        Console.ReadLine() |> ignore     
        
        getpaths () 
            |> recount 
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
                | ConsoleKey.X -> exit 0
                | ConsoleKey.H -> print_help ()
                | _ -> printfn "Unrecognized option."

        loop paths


    [<EntryPoint>]
    let main argv =
        getpaths () |> select_first |> loop
        0 // return an integer exit code
