# Culto

## Instructions

```
$ scalac -cp '*.jar' -d classes score.scala
$ pd instrument-pd/instrument.pd &
$ scala -cp '.:*.jar:classes' score
```

Tested with Scala 2.11.5

![Preview](https://i.imgur.com/b8A7wZS.png)
