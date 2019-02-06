# spinneret-vue

Modifies (patches) and configures the current (QL) version of Spinneret (*another fine product from Ruricolist*) so it becomes comfortable for Vue.js usage (and perhaps other similar libraries).

**This is an evolving library. Suggestions and pull requests welcome!**

## Rationale

Spinneret is a very nice HTML5 generation library, however, it doesn't allow you to create tags of arbitrary name and/or arbitrary arguments. 

So this simple library patches spinneret for working with vue.js components. This will allow:

- Free-form tags where attribute names can be arbitrary.
- Attributes starting with "v-" (i.e. *vbind*) will not trigger a validation warning when used on HTML5-spec tags.
- Attributes starting with "@" and ":" are also allowed.

## Future

When Spinneret supports free-form tags, this library will use them instead of directly patching Spinneret. Thus, it will turn into a configurator for Spinneret.

## Requirements

Spinneret version 2.3 till the current one on Quicklisp (2.7). (spinneret-20170403-git [version 2.3] --> spinneret-20190107-git [version 2.7])

Or a later one that hasn't changed too wildly from that section. This library will patch (modify) a spinneret function, so it previously verifies that all the functions that are involved previously exist. Otherwise it will complain. 

## Usage

**:free-tag**

Creates a free-form tag.

Usage: `(:free-tag :name <component name> [other attributes] <body>)`

*Example:*

```lisp
(with-html-string (:div (:free-tag :name "MyComponent" :v-bind "xx" :id "myId" "CONTENT GOES HERE")))
```

*Output:*

```html
<div>
 <MyComponent v-bind=xx id=myId>
  CONTENT GOES HERE
 </MyComponent>
</div>
```

**component** spinneret tag macro

Let's suppose we're Creating a tag for a Vue.js component "MyComponent".
We'll use spinneret tag macro (deftag) "component". Three usages:

Usage A: Bind some vars to the component.

`(component name <component-name> :bind <list> [attrs] [body])`

Example

```lisp
(component :bind (|var1| "value1" |var2| "value2") :name "MyComponent"  "CONTENT")
```

```html
 <MyComponent v-bind:var2=value2 v-bind:var1=value1>
  CONTENT
 </MyComponent>
```

Usage B: Bind some vars to the component, var values equal to var names.

`(component :name <name> :bind-same <list> [body]))`

Example

```lisp
(component :bind-same (|var1| |var2|) :name "MyComponent"  "CONTENT"))
```

```html
<MyComponent v-bind:var2=var2 v-bind:var1=var1>
 CONTENT
</MyComponent>
```

Usage C: 

`(component :name <name> [attrs] [body]))`

Basically same as using :free-tag.


In any of those examples, the only required attribute is *:name*, any other attribute is optional. In these cases, the attribute names won't be validated so they can be arbitrary, unlike Spinneret's default policy of validating that attributes are conforming to HTML5 spec.

# Thanks to

Ruricolist (Paul M. Rodriguez) for Spinneret.

# Author

Defunkydrummer

Mail me at f_egoavil @ microsoft's ancient free email system

OR 

defunkydrummer@gmail.com



