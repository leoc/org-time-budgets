# Org Time Budgets

This package provides functions to define time budgets per week and
display clocked time in a fancy table.

```elisp
(setq org-time-budgets '((:title "Business" :match "+business" :budget "30:00" :blocks (workday week))
                         (:title "Sideprojects" :match "+personal+project" :budget "14:00" :blocks (day week))
                         (:title "Practice Music" :match "+music+practice" :budget "2:55" :blocks (nil week))
                         (:title "Exercise" :match "+exercise" :budget "5:15" :blocks (day))
                         (:title "Language" :match "+lang" :budget "5:15" :blocks (day week))))
```

Running the function `org-time-budgets-table` will return something like:

```
Business        [|||||.........] 02:47 / 06:00  [||............] 05:46 / 30:00
Sideprojects    [||||..........] 00:36 / 02:00  [|.............] 01:10 / 14:00
Practice Music                                  [|||||.........] 01:04 / 02:55
Exercise        [..............] 00:00 / 00:45
Language        [|||||||||||...] 00:36 / 00:45  [|||...........] 01:10 / 05:15
```

## Defining budget `:blocks`

With the `:blocks` parameter you can define the time blocks to show in
the agenda. It takes a list with any number of entries. Valid entries
are:
* `week` to show the total clocked time this week.
* `day` to show todays budget based on a 7 day week.
* `workday` to show todays budget based on a 5 day week.
* `nil` to display nothing for this block in the budgets table. Use
  this to align your different budgets.

The default value is `(day week)`.

## Adding `org-time-budgets` to your Agenda

You can add your `org-time-budgets` to the top of your `org-agenda` by
doing something like:

```elisp
(setq org-agenda-custom-commands
      '(("a" "Agenda"
         ((agenda "" ((org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))))
          (org-time-budgets-in-agenda-maybe)))))
```

The budgets table can be toggled using <kbd>V</kbd> in the agenda.

## Adjusting display of the budget table

With the variable `org-time-budgets-hide-on-weekend` you can control
whether or not `workday` blocks are also displayed on weekends.

## Contribute

I don't want this thing to die. And I would like to learn cool stuff! :-)

* **Improve performance**
  I would love to learn how to make this package faster!
  Currently I am using default org-mode functions for gathering
  clocked times. Maybe there is a better and more performant way.
* **Add block types**
  Currently time budgets can be per `'day`, `'workday` and `'week`, maybe
  something else is cool too.

## License

GNUv3!
