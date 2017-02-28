# -*- coding: utf-8 -*-


def ngl_prepare_options(args, options):
    parsed_dict = vars(args)

    result = dict()

    for category in options:
        for opt in sorted(options[category]):
            if category not in result:
                result[category] = []

            if parsed_dict[opt] is not None:
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
        print(">>> NGLess will be called with the following script:")
        print(payload)
        print(">>> End of script")

    return payload

# vim: ai sts=4 et sw=4
