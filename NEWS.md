# maybe 1.0.0

Features

- `maybe_map2` Evaluate a binary function on two maybe values
- `maybe_case` Unwrap and call a function on a maybe value or return a default
- `from_just` Unwrap a 'Just' value or throw an error
- `filter_justs` Filter and unwrap a list of 'Just' values
- `filter_map` Map a function over a list and filter only 'Just' values

# maybe 0.2.1

Internal changes

- Now doesn't require R >= 4.1

# maybe 0.2.0

Major breaking changes

- Function named `map_maybe` changed to `maybe_map`
- Function named `flatten_maybe` changed to `maybe_flatten`

Features

- `maybe_contains` checks if a maybe value contains a specific value
- `maybe_equal` checks if two maybe values are identical

# maybe 0.1.0

Initial version
