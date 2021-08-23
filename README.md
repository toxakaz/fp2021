# ФП 2020. Репо для домашек

Домашки по курсу ФП 2020 оформлять **в виде пулл-реквестов к этому репо**.

В директории `/Lambda` лежит шаблон-скелет, его нужно скопипастить и исправить под свои нужды:
- переименовать нужные файлы под свой мини-язык;
- пофикисить имя автора и т.п.
- ну и сделать реализацию с тестами.

Ожидается примерно следующая структура репозитория
- `/Lambda` -- шаблон проекта домашки, который редактирует только препод;
- `/CSharpExc` -- реализация мини-С# c исключениями, на основе шаблона `/Lambda`;
- `/Java` -- реализация мини-Java, снова на основе шаблона `/Lambda`;
- и т.д.

Для Merge Requests настроен CI, который смотрит в какой директории (проекте) произошли последние изменения, 
и именно в этой директории запускает сборку и тесты. Например, если поменялся файл `Lambda/src/Parser.ml`, то запустятся все тесты из директории проекта `Lambda`, а тесты из проекта `Java` запускаться не будут.  Имейте в виду.

###### N.B. Не удаляйте директорию Lambda. Это шаблон!