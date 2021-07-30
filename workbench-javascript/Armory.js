[1,2,3,4,5,6,7,8].reduce(
    (a,b,c,d)=>{
// accumulator, current value, current index, source array
        console.log(a,b,c)
        console.log(d,"\n")
        return a+b+c
    }
)