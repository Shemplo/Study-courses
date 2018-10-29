# Синхронизация с помощью тонкой блокировки

[![Build Status](https://travis-ci.com/ITMO-MPP-2017/fine-grained-bank-Shemplo.svg?token=B2yLGFz6qwxKVjbLm9Ak&branch=master)](https://travis-ci.com/ITMO-MPP-2017/fine-grained-bank-Shemplo)

## Описание
Задание включает в себя следующие исходные файлы:

* `src/main/java/ru/ifmo/pp/Bank.java` содержит интерфейс для гипотетического банка.
* `src/main/java/ru/ifmo/pp/BankImpl.java` содержит реализацию операций банка для однопоточного случая. Данная реализация небезопасна для использования из нескольких потоков одновременно.
* `pom.xml` содержит описание проекта для системы сборки Maven. Используйте его, чтобы создать проект в вашей любимой среде разработке. Рекомендуется IntelliJ IDEA. Используя операцию "File | Import Project" и указав место расположение файла pom.xml, вы создадите проект для выполнения задания.

## Задание
Необходимо доработать реализую `BackImpl` так, чтобы она стала безопасной для использования из множества потоков одновременно.

1.	Для реализации необходимо использовать тонкую блокировку. Синхронизация должна осуществляться для каждого счета по отдельности. Добавьте поле `lock` типа `java.util.concurrent.locks.Lock` (интерфейс для примитива блокировки) в класс `BankImpl.Account` и используйте класс `java.util.concurrent.locks.ReentrantLock` в качестве стандартной реализации.
2.	Для обеспечения линеаризуемости операций должна использоваться двухфазная блокировка для всех операций.
3.	Для избегания ситуации взаимной блокировки (deadlock) необходимо использовать иерархическую блокировку.
4. Весь код должен содержаться в файле `BankImpl.java`. 
5. Код должен быть отформатирован в соответствие со стандартным Java стилем, используя 4 пробела для выравнивания кода. Следуйте стилю, в котором написаны классы `Bank` и `BankImpl`. Плохо отформатированный код не будет проверяться.
6. Перед сдачей задания замените `<your_GitHub_account>` в начале данного файла на свой логин в GitHub для получения информации о сборке в Travis. Это нужно сделать в двух местах: ссылка на картинку и на билд в Travis-е.

## Сборка и тестирование
Проект должен собираться и успешно проходить тестирование с помощью команды `mvn test`. При этом автоматически будут запущены следующие тесты:

* `FunctionalTest.java` проверяет базовую корректность работы операций банка.
* `MTStressTest.java` проверяет основные аспекты корректности работы банка при множестве одновременно работающих потоков исполнения.
* `LinearizabilityTest.java` проверяет все аспекты корректности работы банка и линеаризуемость операций, сравнивая различные варианты одновременного выполнения операций с различными вариантами их перестановки на модельной однопоточной реализации.

Обратите внимание, что исходная реализация проходит только `FunctionalTest`, но не проходит многопоточные тесты. При этом, прохождения тестов недостаточно для зачета за это задание. Тесты будут проходить, если у каждого метода класса `BankImpl` написать ключевое слово `synchronized` (проверьте!). Но такая реализации, несмотря на прохождение тестов, не удовлетворяет заданию, которое требует применение тонкой, а не грубой, блокировки.

## Формат сдачи

Выполняйте задание в этом репозитории. По готовности добавьте "+" в таблицу с оценками в столбец "Готово" текущего задания. 

В случае необходимости доработки домашнего задания после проверки, "+" в таблице замененяется на "?" и создается issue на GitHub-е. Как только необходимые исправления произведены, заменяйте "?" обратно на "+" и закрывайте issue. После этого задание будет проверено ещё раз.

Дедлайн: 19.09.2017

## Оценки и лекции
Таблица с оценками доступна тут: [https://goo.gl/YZrm9Z]()

Лекции доступны на Google Drive: [https://goo.gl/VjKtdK]()