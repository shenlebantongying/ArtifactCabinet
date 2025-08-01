import Foundation

NSLog("Hello, World!");


let names = ["Chris", "Alex", "Ewa", "Barry", "Daniella"]

let reversedNames = names.sorted(by: { (s1: String, s2: String) -> Bool in
	return s1 > s2
})


print(reversedNames)
