class Person(var firstName:String, var lastName: String):
    println("Person init")
    val fullname = firstName+lastName

    def toStr:Unit =
        print("Full name is ")
        println(fullname)
    println("Init end")

@main def hello() =  
    val i = new Person("as","bc")
    i.toStr