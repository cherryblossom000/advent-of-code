-- https://stackoverflow.com/questions/66945893/use-fenced-code-blocks-in-pandoc-markdown-output
function CodeBlock(block)
	return pandoc.RawBlock('markdown', '```none\n' .. block.text .. '\n```\n')
end

function Emph(emph)
	return pandoc.Strong(emph.content)
end

function Link(link)
	link.attr = pandoc.Attr()
	return link
end

doneFirstHeader = false
function Header(header)
	if header.level ~= 2 then return nil end
	if not doneFirstHeader then
		doneFirstHeader = true
		header.content:remove(1) -- ---
		header.content:remove(1) -- Space
		header.content:remove(#header.content) -- ---
		header.content:remove(#header.content) -- Space
		dayText = header.content[3].text
		return pandoc.Header(1, pandoc.Link(header.content, 'https://adventofcode.com/2023/day/' .. dayText:sub(1, #dayText - 1)))
	end
	return pandoc.Header(2, 'Part Two')
end
