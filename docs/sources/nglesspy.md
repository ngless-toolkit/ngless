# NGLessPy: NGLess in Python

**Note** As of Oct 2017, NGLess is considered beta software (we believe it
works, but there may still be a few rough edges), while NGLessPy is alpha
software (very experimental).

## Install

```bash
    pip install NGLessPy
```

(or from source, using the standard `python setup.py install`)

## Basic Tutorial

This tutorial expects a certain familiarity with general ngless concepts and
functions.

We start by importing the `NGLess` object:

```python
    from ngless import NGLess
```

We now build an `NGLess.NGLess` object, giving it the version of ngless we wish
(this is like the version declaration at the top of an NGLess file:


```python
    sc = NGLess.NGLess('0.8')
```

To simplify the rest of the script, we are going to use the short name `e` to
refer to the environment of the script we are generating. The environment is
what will hold the ngless variables we will use:

```python
    e = sc.env
```

We can import ngless modules using the `import_` function (using `name` and
`version`):

```python
    sc.import_('mocat', '0.0')
```

Now, we can use all NGLesss functionality. Functions get an underscore at the
end, like this:

```python
    e.sample = sc.load_mocat_sample_('testing')
```

`preprocess_` is special because it takes a block in ngless, which maps to it
taking a function in Python. We can use decorator syntax to do it all
compactly:

```python
    @sc.preprocess_(e.sample, using='r')
    def proc(bk):
        # bk is the block environment, where `r` is defined
        bk.r = sc.substrim_(bk.r, min_quality=25)
```

Now, we map against `hg19` and filter it. As you can see, ngless functions are
called with an extra underscore and variables are kept in the `e` object:

```python
    e.mapped = sc.map_(e.sample, reference='hg19')
    e.mapped = sc.select_(e.mapped, keep_if=['{mapped}'])

    sc.write_(e.mapped, ofile='ofile.sam')
```

Finally, we execute the resulting script:

```python
    sc.run(auto_install=True)
```

This will even install NGLess if it is not available in the PATH.

## Full script


```python
    from ngless import NGLess

    sc = NGLess.NGLess('0.8')
    e = sc.env

    sc.import_('mocat', '0.0')

    e.sample = sc.load_mocat_sample_('testing')
    @sc.preprocess_(e.sample, using='r')
    def proc(bk):
        # bk is the block environment, where `r` is defined
        bk.r = sc.substrim_(bk.r, min_quality=25)

    e.mapped = sc.map_(e.sample, reference='hg19')
    e.mapped = sc.select_(e.mapped, keep_if=['{mapped}'])

    sc.write_(e.mapped, ofile='ofile.sam')

    sc.run(auto_install=True)
```

