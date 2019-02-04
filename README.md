# spinneret-vue

Modifies (patches) Spinneret (*another fine product from Ruricolist*) so it becomes comfortable for Vue.js usage (and other similar libraries)

## Rationale

Spinneret is a very nice HTML5 generation library, however, it doesn't allow you to create tags of arbitrary name and/or arbitrary arguments. 

So this simple library patches spinneret for working with vue.js components, or other similar HTML+JS libraries.

## Requirements

Spinneret version 2.3 or a later one that doesn't change too wildly. This library will patch (modify) a spinneret function, so it previously verifies that all the functions that are involved previously exist. Otherwise it will complain. 

## Usage

Creating a tag for a component MyComponent: Use tag macro: component :name <component-name>  [attrs] <body>

```lisp
 (with-html-string (:div (component :name "MyComponent" :v-bind "xx" :id "myId")))
```

```html
<div>
 <MyComponent v-bind=xx id=myId>
 </MyComponent>
</div>
```

Creating a free-form tag (almost identical to above, really)
Usage: (:free-tag :name <component name> [other attributes] <body>)

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

In any of those examples, the only required attribute is *:name*, any other attribute is optional. In these cases, the attribute names won't be validated so they can be arbitrary, unlike Spinneret's default policy of validating that attributes are conforming to HTML5 spec.

# Credits

Ruricolist for Spinneret.

# Author

Defunkydrummer

Mail me at f_egoavil @ microsoft's ancient free email system

OR 

defunkydrummer@gmail.com



