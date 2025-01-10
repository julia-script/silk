package main
import "fmt"
type T struct {
    a int
}
func main() {
	var a T
	a.a = 1

	var b T
	b = a
	
	b.a = 3
	fmt.Println(a.a)
	fmt.Println(b.a)
}
