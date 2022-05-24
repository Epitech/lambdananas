# Lambdananas

![pineapple](https://user-images.githubusercontent.com/102175969/166650736-40d20379-fe54-4f79-9f27-f74ceaeedef6.png)

The haskell coding style checker, version 2.

> Was previously called hsc or haskell-language-checker

## 🙋 Contribute

### Commit format

When writing a commit, please use the following format :

- A description of less than 80 characters
- Why the changes were made / how were they made (optional)

Please use multi line commits when adding descriptions. 

#### Example of a correct commit:

```
Update system tests script removing build step
    
We should stack build with --exec. That is the
proper way to execute something at the end of a build
else it will get very confused and exit before
finishing.
   
The script is needed because we cannot launch
shelltest within exec for reasons that i don't
quite understand...
```

## 📦 Build & Run

### Using stack

- Run `stack build`
- You can run the checker for tests using `stack run -- [arguments]`

### Using make

You can use commands specified by Epitech ($NAME re clean fclean tests_run)

## 🔧 Tests

- Run `stack test` to run all integration and unit tests
- Install `shelltest` and run `shelltest -c test` from the root of the repo to run cli tests

## 📂 Module hierarchy
![haskell style checker module tree](https://user-images.githubusercontent.com/102175969/169989347-3fc76e11-eea4-450a-9068-de62d064b911.png)
