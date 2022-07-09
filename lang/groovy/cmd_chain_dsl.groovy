show = {println it}
sqr_root = {Math.sqrt(it)}

def please(action){
    [the: {what ->
        [of: { n-> action(what(n))}]
    }]
}

please show the sqr_root of 100

please(show).the(sqr_root).of(100)

