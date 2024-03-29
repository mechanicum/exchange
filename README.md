# Запуск
`sbt run` - запуск по умолчанию

`sbt "run out=./target/res.txt"` - в параметр out передаётся полное имя файла в который сохранять результат, если не устраивает умолчательное

# Запуск тестов
`sbt test`

# Расположение входных файлов:
```
./src/main/resources/clients.txt
./src/main/resources/orders.txt
```

# Расположение выходного файла
`./target/result.txt`

# Исправленные несоответствия в постановке задачи
* Раздел "Требования к матчингу", пункт "Балансы клиентов не могут уходить в минус" - в списке заявок поменяны местами цена и количество
* Раздел "Требования к матчингу", пункт "Реализовать частичное сопоставление заявок" - в списке заявок поменяны местами цена и количество

# Описание задачи

Напишите программу для простейшего матчинга заявок на упрощенной бирже. На бирже торгуется четыре ценных бумаги (_A_, _B_, _C_ и _D_). Базовая валюта биржи - доллар (_$_). 

Входные данные для программы содержатся в двух файлах. Первый файл (`clients.txt`) содержит список клиентов биржи с указанием их исходных балансов по торгующимся ценным бумагам и доллару. Второй файл (`orders.txt`) — это список заявок от клиентов в хронологическом порядке. Результатом работы программы является файл `result.txt` аналогичный по структуре файлу `clients.txt` и содержащий состояние балансов всех клиетов после обработки всех заявок.

Реализация должна быть на Scala.

В текущей реализации биржа работает по принципу обычной очереди FIFO, т.е. в приоритете на закрытие заявка которая раньше попала на биржу.

# Описание форматов файлов

Каждая строка тесктового файла содержит одну запись. Поля записи отделяются друг от друга с помощью символа табуляции (`\t`). Имена клиентов, названия ценных бумаг - строки, состоящие из буквенных и цифровых символов ASCII без разделителей. Числовые значения представлены целыми числами. 

## Файл `clients.txt`

Файл списка клиетов имеет следующие поля:
 * Имя клиента
 * Баланс клиента по долларам 
 * Баланс клиента по ценной бумаге _A_ в штуках
 * Баланс по ценной бумаге _B_
 * Баланс по ценной бумаге _C_
 * Баланс по ценной бумаге _D_

Пример нескольких строк файла:

```
C1  1000    10  5   15  0 
C2  2000    3   35  40  10
```

## Файл `orders.txt`

Файл списка заявок имеет формат:

 * Имя клиента выставившего заявку
 * Символ операции: `s` - продажа или `b` - покупка.
 * Наименование ценной бумаги
 * Цена заявки (целое число за одну штуку ценной бумаги)
 * Количество продаваемых или покупаемых ценных бумаг
 
Пример нескольких строк файла:

```
C1  b   A   10  12
C2  s   A   8   10
```

# Требования к матчингу
 * Состояние счетов клиентов можно обрабатывать без транзакций.
 * Балансы клиентов не могут уходить в минус. Заявки, не обеспеченные нужным количеством валюты (при покупке) или ценных бумаг (при продаже), должны отвергаться. Например:
   ```
   C1  1000    10  5   15  0 
   C2  2000    3   35  40  10
   ```
   ```
   C1   b   A   150  10 (1)
   C2   s   B   10  40  (2)
   ```
   Заявка (1) должна быть отвергнута, потому что у С1 недостаточно валюты для того, чтобы купить 10 бумаг _A_ по цене 150$. Заявка (2) должна быть отвергнута, потому что у С2 есть только 35 бумаг _В_.
 * Реализовать частичное сопоставление заявок: 
   ```
   C1   b   A   12  10 (1)
   C2   s   A   10   8 (2)
   ```
   Заявка 1 и 2 должна частично сопоставиться т.е. заявка 2 полностью погасится, заявка 1 погасится частично и изменит стейт на  `C1  b   A   2   12`.
 * Заявки на исполнение по одной цене должны исполняться в порядке поступления:
   ```
   C1   b   A   10  10 (1)
   C2   b   A   10  10 (2)
   C3   s   A   10  15 (3)
   ```
   В такой ситуации сначала сопоставятся заявки (3) и (1), (1) полностью погасится, а затем остаток от (3) сопоставится с заявкой (2).
  

# Ожидаемые результаты

 * Основной результат - программа, которая обработав данные клиентов и список заявок (`clients.txt` и `orders.txt`) генерирует файл конечного состояния счетов клиентов (`results.txt`).
    - программа должна запускаться через `sbt run` (или любым другим способом, описанном в README)
 * Исходный код проекта на Github
 * Набор unit-тестов (тесты, естественно, должны проходить)
