// an object is a collection of zero or more properties each with attributes that determine how each property can be used

// Attach property at Run time
let o = {};
o.a_action=function (a_arg){
    console.log(`[o] say ${a_arg}`);
}

o.a_action("yes")

// detach property at run time
delete o.a_action
// Error o.a_action()
console.log(o.toString()) //=> {}

console.log("=======")

let o2 = {
    a_action(something){
        console.log(`[${this.type}] says ${something}`)
    }
}

let my_o2 = Object.create(o2);
o2.type = "o2Type";
o2.a_action("No");
console.log(o2)

// Object factory
function make_o2(type){
    let _o2 = Object.create(o2)
    _o2.type="NewO2Type"
    return _o2
}

let my_new_o2 = make_o2("candy")
console.log(my_new_o2)
my_new_o2.a_action("new_o2")



