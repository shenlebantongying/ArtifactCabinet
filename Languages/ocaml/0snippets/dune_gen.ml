print_string {|(executables
 (names|};;

Sys.readdir "."
|> Array.to_list
|> List.filter (fun name -> Filename.check_suffix name ".ml")
|> List.map Filename.remove_extension
|> List.sort String.compare
|> List.iter (Printf.printf "\n  %s")
;;

print_string "))\n";;

print_string {|
(env
 (dev
  (flags
   (:standard -w -a))))
|}
