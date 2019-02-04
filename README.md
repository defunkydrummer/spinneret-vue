# spinneret-vue

Modifies (patches) Spinneret (*another fine product from Ruricolist*) so it becomes comfortable for Vue.js usage (and other similar libraries)

## Rationale

Spinneret is a very nice HTML5 generation library, however, it doesn't allow you to create tags of arbitrary name and/or arbitrary arguments. 

So this simple library patches spinneret for working with vue.js components, or other similar HTML+JS libraries.

## Requirements

Spinneret version 2.3 or a later one that doesn't change too wildly. This library will patch (modify) a spinneret function, so it previously verifies that all the functions that are involved previously exist. Otherwise it will complain. 

## Usage

*:free-tag*

Creates a free-form tag.

Usage: `(:free-tag :name <component name> [other attributes] <body>)`

```lisp
(with-html-string (:div (:free-tag :name "MyComponent" :v-bind "xx" :id "myId" "CONTENT GOES HERE")))
```

```html
<div>
 <MyComponent v-bind=xx id=myId>
  CONTENT GOES HERE
 </MyComponent>
</div>
```

*component* spinneret tag macro

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

# Credits

Ruricolist for Spinneret.

# Author

Defunkydrummer

Mail me at f_egoavil @ microsoft's ancient free email system

OR 

defunkydrummer@gmail.com



