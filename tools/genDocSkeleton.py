import re

f = open('server.sml')
lines = f.readlines()

output = []

for line in lines:
    definition = re.search('(\ *)val ([a-zA-Z0-9]+)\ +:\ +(.+)', line)

    if definition:
        ident = definition.group(1)
        name = definition.group(2)
        fType = definition.group(3)

        spec = [
        '',
        '(*',
        '    %s' % name,
        '    TYPE: %s' % fType,
        '    PRE: (none)',
        '    POST: (none)',
        '    SIDE-EFFECTS: (none)',
        '    EXAMPLE: ',
        '*)']

        spec = map((lambda x: ident + x), spec)
        spec.append(line)

        output.append("\n".join(spec))
    else:
        output.append(line)

f.close()

f = open('server-spec.sml', 'w+')
f.write("".join(output))
f.close()