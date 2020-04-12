# Org Time Budgets

This package provides functions to define time budgets per week and
display clocked time in a fancy table.

```elisp
(setq org-time-budgets '((:title "Business" :tags "+business" :budget "30:00" :block workweek)
                         (:title "Sideprojects" :tags "+personal+project" :budget "14:00" :block week)
                         (:title "Practice Music" :tags "+music+practice" :budget "2:55" :block week)
                         (:title "Exercise" :tags "+exercise" :budget "5:15" :block week)
                         (:title "Language" :tags "+lang" :budget "5:15" :block week)))
```

Running the function `org-time-budgets-table` will return something like:

```
Business        [|||||.........] 02:47 / 06:00  [||............] 05:46 / 30:00
Sideprojects    [||||..........] 00:36 / 02:00  [|.............] 01:10 / 14:00
Practice Music  [||||||||||||||] 01:04 / 00:25  [|||||.........] 01:04 / 02:55
Exercise        [..............] 00:00 / 00:45  [..............] 00:00 / 05:15
Language        [|||||||||||...] 00:36 / 00:45  [|||...........] 01:10 / 05:15
```

## Adding `org-time-budgets` to your Agenda

You can add your `org-time-budgets` to the top of your `org-agenda` by
doing something like:

```elisp
(setq org-agenda-custom-commands
      '(("a" "Agenda"
         ((agenda "" ((org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))))
          (org-time-budgets-in-agenda)))))
```

## Contribute

I don't want this thing to die. And I would like to learn cool stuff! :-)

* **Improve performance**
  I would love to learn how to make this package faster!
  Currently I am using default org-mode functions for gathering
  clocked times. Maybe there is a better and more performant way.
* **Add block types**
  Currently time budgets can be per `'workweek` and `'week`, maybe
  something else is cool too.
* **Fix font-locking in agenda**

## License

GNUv3!
