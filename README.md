A basic program that allows calculations with units.

All calculations will be done with either units or quantities. Units are simply data that represents things like "meters" or "seconds" or "meters per second". Quantities are a value plus units, such as "1 m/s".

To convert a quantity from one unit to another, simply use the "convert" function as follows:

```
convert (Quantity (meter / second) 1) (mile / hour)
```

To calculate with quantities, you can simply use the normal arithmetic operators. For example:

```
(Quantity (meter / second) 2) + (Quantity 4 (foot / second))
```

If the units of the first and second quantities in addition or subtraction are different, then the units of the first will automatically be converted to the units of the second. If writing out Quantity and such takes too long, you can use either the `parseQuantity` or simply `q` function, which takes a string and returns a quantity if it can be parsed. This can be used as follows to do the same calculation as above as follows:

```
q "2 m/s" + q "4 ft/s"
```
