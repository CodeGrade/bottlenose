## Templates

By default Rails associates each directory in `app/views/` with a controller. For example the controller `CoursesController` will look for view in `app/views/courses/` by default. The files in each directory here are of two kinds, either action templates, or partials. Action templates are the files which Rails uses to render a complete page (within a layout). For example the route `courses#index` points to the `CoursesController` and asks to call the `index` method on it. This then by default renders the template located at `app/views/courses/index.html.erb`. The complete process is actually a bit more complicated, but this is generally how we use it. A partial is a file prepended with an underscore like `_form.html.erb`, and is used to modularize views.

Generally we'll have the following views for each controller.

- `index.html.erb` lists all the records (might be scoped).
- `new.html.erb` gives a form for creating a *new* record.
- `show.html.erb` shows the record and any related data.
- `edit.html.erb` gives a form for editing an *existing* record.

To make things more modular we should have the following partials defined for each model.

- `_form.html.erb` a form used for creating **and** updating records. The local variable by the pluralized name of the corresponding model should hold the appropriate iterable collection of records.
- `_table.html.erb` a table view for listing all records (might be scoped).

## Layouts

The directory `app/views/layouts` is the default location for templates in Rails. A Rails layout is a wrapper of HTML that each template is embedded within. The layout `app/views/layouts/application.html.erb` is the main layout, and handles things like the navbar, footer, and `<head>` section.

## Mailers

This directory also serves the view templates for the mailers defined in `app/mailers`. This works the same way as the controllers, but for rendering email HTML.
