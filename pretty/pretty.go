package pretty

import (
	"fmt"
	"io"
	"os"
	"reflect"
	"strconv"
)

func Print(val interface{}) {
	Fprint(os.Stdout, val)
}

func Fprint(w io.Writer, val interface{}) {
	printIndented(w, val, 0, 2, false)
	fmt.Fprint(w, "\n")
}

func maybePrintConversion(w io.Writer, ty reflect.Type) func() {
	if ty.PkgPath() != "" {
		fmt.Fprint(w, ty.String()+"(")
		return func() {
			fmt.Fprint(w, ")")
		}
	} else {
		return func() {}
	}
}

func isSimpleType(ty reflect.Type) bool {
	switch ty.Kind() {
	case reflect.Bool,
		reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Uintptr, reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128:
		return true
	default:
		return false
	}
}

func printIndented(w io.Writer, val interface{}, indent, ptrdepth int, aggElem bool) {
	if val == nil {
		fmt.Print("nil")
	}

	nl := func() {
		fmt.Fprint(w, "\n")
		for i := 0; i != indent; i++ {
			fmt.Fprintf(w, "\t")
		}
	}

	ty := reflect.TypeOf(val)
	switch ty.Kind() {
	case reflect.Bool,
		reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Uintptr, reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128:
		defer maybePrintConversion(w, ty)()
		fmt.Fprint(w, val)

	case reflect.Array:
		if !aggElem {
			fmt.Fprint(w, ty.String())
		}
		fmt.Fprint(w, "{")
		if isSimpleType(ty.Elem()) {
			for i := 0; i != ty.Len(); i++ {
				if i != 0 {
					fmt.Fprint(w, ", ")
				}
				fmt.Fprint(w, reflect.ValueOf(val).Index(i).Interface())
			}
		} else {
			indent++
			for i := 0; i != ty.Len(); i++ {
				nl()
				printIndented(w, reflect.ValueOf(val).Index(i).Interface(), indent, ptrdepth, true)
				fmt.Fprint(w, ",")
			}
			indent--
			nl()
		}
		fmt.Fprint(w, "}")

	case reflect.String:
		defer maybePrintConversion(w, ty)()
		fmt.Fprint(w, strconv.Quote(val.(string)))
	}
}
