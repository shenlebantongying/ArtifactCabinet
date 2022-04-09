class event =
object
    method execute = (); 
end

class frenchEvent =
object
  inherit event
  method! execute =
    print_string "Bonjure!"
end;;

let trigger (e:event) =
  e#execute in
let frenchEventObj = new frenchEvent in
  trigger frenchEventObj;