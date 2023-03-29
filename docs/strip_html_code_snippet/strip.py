import bs4

with open('code.srs') as handle:
    code  = bs4.BeautifulSoup(handle.read(), 'html.parser')

new_code = '<code class = "block">'

for line in code:
    for token in line:
        if type(token) == str:
            new_code += token
        else:
            new_code += '<span class = "'
            try:
                for cl in token['class']:
                    new_code += cl + ' '
            except:
                new_code += '\n'
            new_code = new_code[:-1] + '">'

            new_code += token.get_text()
            new_code += '</span>'
            
new_code += '</code>'

with open('new_code.srs', 'w') as handle:
    handle.write(new_code)