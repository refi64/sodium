Спецификация подмножества языка Паскаль
=======================================

Типы данных
-----------

* Целые числа
* Действительные числа
* Логические значения
* Строки


Возможности
-----------

* Операторы `+`, `-`, `*`, `/`
* Операторы `div`, `mod`
* Операторы `>`, `<`, `=`
* Операторы `and`, `or`, `not`
* Присваивание
* Условный оператор
* Цикл `for`
* Функции
* Ввод и вывод


Ограничения
-----------

Процедура ввода `ReadLn` может иметь только один
параметр.

Все функции должны быть чистыми, а именно:

* параметры передаются только по значению
* нет доступа к глобальным переменным
* нет доступа к подсистеме ввода-вывода

Пример:

    var
      n: Integer;
      sum: Real;
    begin
    sum := 0;
    ReadLn(n);
    for i := 1 to n do
      sum := sum + i;
    WriteLn(sum/n);
    end.
