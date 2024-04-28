package main

import (
	"fmt"
)

func c() (i int) {
	j := 14
    defer func() { 
		j++
		fmt.Println(j)
		 }()
	fmt.Println(j)
    return 23
}


func main()  {
	fmt.Println("Hello World!!")
	fmt.Println(c())
}

/*
Go function can return multiple values
Go synxtax for declaration is name and then type
Go function returns values can have names
Short variable declaration ":=" is only available insde function
Basic types -
	bool
	string
	int  int8  int16  int32  int64
	uint uint8 uint16 uint32 uint64 uintptr
	byte // alias for uint8
	rune // alias for int32
		// represents a Unicode code point
	float32 float64
	complex64 complex128
	
constant can be created with const keyword
short syntax does not work with const

*/
