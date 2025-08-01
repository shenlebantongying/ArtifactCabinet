class Person {
    constructor(first, last , id) {
        this.name={
            first,
            last
        }
        this.id=id
    }

    greeting(){
        console.log(`This is ${this.name.last}`)
    }
}

let john = new Person('john','lkj',123)
john.greeting()
