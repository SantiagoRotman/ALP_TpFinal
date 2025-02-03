# Prolog

Código para la materia ALP 2023 de [LCC](https://dcc.fceia.unr.edu.ar), [FCEIA](https://www.fceia.unr.edu.ar), [UNR](https://www.unr.edu.ar).

## Intérprete Prolog

Para correr el intérprete en modo interactivo basta con ejecutar:

```code
cabal run
```

O para correr un programa

```code
cabal run -- -e <file>
```

Las opciones soportadas por el intérprete de Prolog pueden verse utilizando el comando `:help` :

```code
> :help
Lista de comandos:  Cualquier comando puede ser abreviado a :c donde
c es el primer caracter del nombre completo.

<expr>                  evaluar la expresión
:load <file>            Cargar un programa desde un archivo
:print <exp>            Imprime un expresión y sus ASTs sin evaluarlo
:trace <on/off>         Activa/Desactiva la traza para debuggear
:quit, :Q               Salir del intérprete
:help, :?               Mostrar esta lista de comandos
```

## Entorno interactivo con GHCi

También pueden cargar un módulo específico del proyecto en el entorno interactivo GHCi:

```code
cabal exec ghci -- -isrc src/Parse.hs
```

La bandera `-isrc` es necesaria para indicarle a GHCi que los archivos que importa el módulo que
estamos cargando deben ser buscados dentro de la carpeta `src/`.

