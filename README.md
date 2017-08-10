# [hexy][]


-   Add a category to `package.yaml`. A list of categories is available on
    Hackage at <http://hackage.haskell.org/packages>.

-   Rename `library/Example.hs` to whatever you want your top-level module to
    be called. Typically this is the same as your package name but in
    `CamelCase` instead of `kebab-case`.

    -   Don't forget to rename the reference to it in
        `executable/Main.hs`!

-   If you are on an older version of Stack (<1.0.4), delete `package.yaml` and
    remove `/*.cabal` from your `.gitignore`.

Once you've done that, start working on your project with the Stack commands
you know and love.

``` sh
# Build the project.
stack build

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock
```

Thanks again, and happy hacking!

[hexy]: https://github.com/jship/hexy
