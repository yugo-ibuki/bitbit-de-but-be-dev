# Basic Page Layout

This is an example of a basic documentation page layout. It uses the default VitePress theme with a simple structure.

## Headers and Text

Headers are created using Markdown syntax. You can use `#` for h1, `##` for h2, and so on.

Regular paragraphs are written as plain text. You can use **bold text** with double asterisks, *italic text* with single asterisks, and `inline code` with backticks.

## Lists

Unordered lists:

- Item 1
- Item 2
- Item 3
  - Nested item 1
  - Nested item 2

Ordered lists:

1. First item
2. Second item
3. Third item

## Code Blocks

```js
// JavaScript code example
function greeting(name) {
  return `Hello, ${name}!`;
}

console.log(greeting('World'));
```

## Blockquotes

> This is a blockquote. It can span multiple lines and can contain other Markdown elements.
>
> Second paragraph in the blockquote.

## Tables

| Name  | Type    | Description           |
|-------|---------|-----------------------|
| id    | string  | Unique identifier     |
| name  | string  | User's display name   |
| email | string  | User's email address  |
| age   | number  | User's age in years   |

## Links and Images

[Link to VitePress documentation](https://vitepress.dev/)

![VitePress Logo](https://vitepress.dev/vitepress-logo-large.webp)

## Dividers

---

## Custom Containers

::: info
This is an info box.
:::

::: tip
This is a tip.
:::

::: warning
This is a warning.
:::

::: danger
This is a dangerous warning.
:::

::: details
This is a details block, which does not work in all browsers.
:::