# Liva VM

The backend of the Liva programming language.
It is a bytecode compiler and VM written in `Rust`.
Generally is Liva a mix between `Python` and `Lua`.


It will be a object oriented programming language with fast development time.

**Some sample code**:

```lua
fun collatz(n)
    println(n)
    if n == 1 do
        return 1
    end
    
    if n % 2 == 0 do
        return collatz(n / 2)
    else 
        return collatz(3 * n + 1)
    end

    return
end

collatz(256)
```


## TODOS:

* [  ] Adding imports
* [  ] Try getting some bindings to work (pytorch, some graphics lib)
* [  ] Multiple returns
* [  ] Cover all suffix calls (to make method calls work, aswell as index operations)
* [  ] Make it object oriented, now lose rust types as backend

