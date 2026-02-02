# Manual

## Demos

### Basic Operations

Insert, query, delete keys and get root hash:

```asciinema-player
{
    "file": "assets/asciinema/basic-ops.cast",
    "idle_time_limit": 2,
    "theme": "monokai",
    "poster": "npt:0:3"
}
```

### Proof Operations

Generate and verify self-contained inclusion proofs:

```asciinema-player
{
    "file": "assets/asciinema/proof-ops.cast",
    "idle_time_limit": 2,
    "theme": "monokai",
    "poster": "npt:0:3"
}
```

## CLI Usage

The CSMT library comes with a command-line interface (CLI) tool that allows users to interact with the Compact Sparse Merkle Tree. The CLI provides various commands to perform operations such as adding and removing elements, generating proofs, and verifying membership within the tree.

CLI works in interactive mode by default. You can also pass commands directly as stdin as we are doing in this manual.


### List of commands
| Command | Description                        | Arguments        | Return value                 |
| ------- | ---------------------------------- | ---------------- | ---------------------------- |
| `i`     | Insert a key-value pair            | key,  value      |                              |
| `d`     | Delete a key                       | key              |                              |
| `q`     | Query inclusion proof for a key    | key              | base64 encoding of the proof |
| `r`     | Get the current root of the CSMT   |                  | base64 encoding of the root  |
| `v`     | Verify inclusion proof             | proof            | Valid or Invalid             |
| `w`     | Query value for a key              | key              | value                        |
| `c`     | Comment (no operation)             |                  |                              |

## Basic operations

### Setup
Setup the environment variable `CSMT_DB_PATH` to point to a directory where the CSMT will store its data. For example:

=== "Input"
    ```bash
    export CSMT_DB_PATH=tmp/demo
    rm -rf $CSMT_DB_PATH
    ```

### Insertion

=== "Input"
    ```bash
    csmt <<$
    i key1 value1
    q key1
    $
    ```
=== "Output"
    ```text
    AQDjun1C8tTl1kdY1oon8sAQWL86/UMiJyZFswQ9Sf49XQAA
    ```

The `output` is the inclusion proof for `key1`. It will not change depending on the value associated with the key.

Now the database contains the value for `key1`, and you can query its inclusion proof at any time.

### Verification

=== "Input"
    ```bash
    csmt <<$
    v AQDjun1C8tTl1kdY1oon8sAQWL86/UMiJyZFswQ9Sf49XQAA
    $
    ```
=== "Output"
    ```text
    Valid
    ```

The proof is self-contained and includes the value hash, so verification doesn't need the value as a separate argument.

### Querying

Currently you cannot inspect the keys, but you can ask for the values:

=== "Input"
    ```bash
    csmt <<< 'w key1'
    ```
=== "Output"
    ```text
    value1
    ```

### Deletion

You can delete keys as well:

=== "Input"
    ```bash
    csmt <<< 'd key1'
    ```
=== "Output"
    ```text
    DeletedKey
    ```
Now if you try to query for the inclusion proof of `key1` again:
=== "Input"
    ```bash
    csmt <<< 'q key1'
    ```
=== "Output"
    ```text
    NoProofFound
    ```



Or if you try to get the value for `key1`:

=== "Input"
    ```bash
    csmt <<< 'w key1'
    ```
=== "Output"
    ```text
    KeyNotFound
    ```

### Getting the root

You can get the current root of the CSMT tree with the `r` command:

=== "Input"
    ```bash
    csmt <<< 'r'
    ```
=== "Output"
    ```text
    TreeEmpty
    ```

If you insert some keys first:

=== "Input"
    ```bash
    csmt <<$
    i key1 value1
    r
    i key2 value2
    r
    d key2
    r
    d key1
    r
    $
    ```
=== "Output"
    ```text
    AddedKey
    NrJMih3czFriydMUwvFKFK6VYKZYVjKpKGe1WC4e+VU=
    AddedKey
    jyW/0W96OAsUNpbd+SgA0B/ZjM8zGBOd3xR5Y1iOJOs=
    DeletedKey
    NrJMih3czFriydMUwvFKFK6VYKZYVjKpKGe1WC4e+VU=
    TreeEmpty
    ```