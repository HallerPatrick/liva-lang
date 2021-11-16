

## Variables

```
let x = 3

let x = []

let x = {}
```

## Functions 

```
fn foo(x)
    return x
end
```

## Classes and Methods

```

class Foo:
end

interface IBar:
    fn foo(x)
end

class Bar extends Foo implements IBar

    fn _init() 
        &m = 3
    end
    
    fn foo(self):
        print(&m)  # 3
    end
    
    
    fn _str():
    end
    
    fn _int()
    end
end

companion Bar
    fn companion_func(x):
    end
    
    fn bar(y)
        &m // this is not allowed
        return y
    end
end

b = Bar()
b.foo()

b::bar(x)
Bar::bar(x)

b.injected_func = \(x) -> print(x)
```

## Loops/While

```
for x in y:
    print(x)
end
```

```
while x < y:
    print(x)
end
```

## If Else

```
if x == 3:
    print(x)
else
    print("Not " + x)
end
```


## Features


Apply


```
list = []

apply(
    list,
    map: \(x) -> x**x,
    filter: \(x) -> x % 2 == 0
)

```

```
```
