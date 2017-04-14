# -*- coding: utf-8 -*-


def ngl_prepare_options(args, options):
    cmd_line_args = vars(args)

    result = dict()

    for category in options:
        for opt in sorted(options[category]):
            if category not in result:
                result[category] = []

            if opt in cmd_line_args:
                if cmd_line_args[opt] is not None:
                    if callable(options[category][opt]):
                        result[category].append(options[category][opt](cmd_line_args, opt))
                    else:
                        result[category].append(options[category][opt])

    # Format list of options to string by joining options with commas
    for category in result.keys():
        result[category] = ", ".join(result[category])

    # Ensure all categories exist in the final dict even if empty
    for category in options:
        if category not in result:
            result[category] = ''

    if args.debug:
        from pprint import pprint
        print(">>> Options processed internally")
        print(args)
        pprint(result)
        print(">>> End of internal options")

    return result


def ngl_prepare_payload(args, payload_tmpl):
    payload = payload_tmpl.format(**vars(args))

    if args.debug:
        print(">>> DEBUG mode active, ngless will not be launched")
        print(">>> NGLess would have been called with the following script:")
        print(payload)
        print(">>> End of script")
        import sys
        sys.exit(0)

    return payload


def _escape_wrap(wrap):
    """If the wrap characters are { or } we need to escape them since this is
    what python uses for .format() targets.
    """
    def escape(char):
        if char == "{":
            return "{{"
        elif char == "}":
            return "}}"
        else:
            return char

    if wrap:
        if len(wrap) == 1:
            char = escape(wrap[0])
            return (char, char)
        else:
            return (escape(wrap[0]), escape(wrap[1]))
    else:
        return ('', '')


def ngl_as_list(template, wrap_with=None):
    """Transforms a multiple choice (nargs) command-line option into a list of
    template elements

    *wrap_with* should be an iterable with 1 or 2 elements.
    If 2 elements they will be used as left and right wrap characters, respectively.
    If 1 element the character will be used for both left and right.

    Example:
        template = "{target}"
        arguments = {"target": ["one", "two"]}
        output = ngl_as_list(template, wrap_with="<>")(arguments, "target")
        output == "[<one>, <two>]"
    """
    def wrap(x):
        return "{wrap[0]}{0}{wrap[1]}".format(x, wrap=_escape_wrap(wrap_with))

    def lazy(arguments, target):
        output = []

        for arg in arguments[target]:
            output.append(wrap(template.format(**{target: arg})))

        return "[{0}]".format(", ".join(output))
    return lazy

# vim: ai sts=4 et sw=4
