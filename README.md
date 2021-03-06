If you want a lispy-way to define simple templates of varying size
with multiple variables, then you should really consider
terrible-template for all your templating needs!

Features
========

This library features a familiar, lispy syntax designed to make
defining templates easy and intuitive for users of emacs. Supporting
multiple template variables and interactive value prompting,
terrible-template should help you to define templates and insert them
easily as part of your workflow.

This was inspired by things such as:


* Templ Mode http://www.emacswiki.org/emacs/TempoMode
* Skeleton Mode http://www.emacswiki.org/emacs/SkeletonMode

but with a simpler interface and the ability to easily use multiple
template variables.

Example
=======

Observe an example:

    (defterrible "my-template-name"
        "First line of the template\n"
        "I'd like a variable here: " (var "var1") " as a demo.\n"
        "Hooray for more variables resembling " (var "var2") " this right here")

You can then insert this template anywhere with `M-x
terrible-template-insert <RET> my-template-name` which will prompt you
for the variable values, interpolate the template and insert it at
point.

Non-interactive example
-----------------------

You can still leverage terrible-template non-interactively with the
`terrible-template-programmatic-apply` function like so:

    (terrible-template-programmatic-apply "my-template-name" '("key1" "val1") '("key1" "val1"))

This will return a string containing the interpolated template which
you may then use for whatever you desire.

Credits
=======

I owe much to the wonderful folks on #emacs who helped me get my head
straight for this little project.
