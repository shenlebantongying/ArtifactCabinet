function curry(f) {
    return function (a) {
        return function (b) {
            return f(a, b);
        }
    }
}

function add(a, b) {
    return a + b
}

console.log(
    curry(add)(1)(2)
)
