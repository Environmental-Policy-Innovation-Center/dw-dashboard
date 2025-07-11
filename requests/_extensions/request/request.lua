local pandoc = require 'pandoc'

return {
  ['request'] = function(args, kwargs, meta)
    local tag = pandoc.utils.stringify(args[1] or "")
    local date = pandoc.utils.stringify(args[2] or "")
    local source = pandoc.utils.stringify(args[3] or "")
    local requested_by = pandoc.utils.stringify(args[4] or "")
    local summary_raw = pandoc.utils.stringify(args[5] or "")
    local resolution_raw = pandoc.utils.stringify(args[6] or "")

    -- Replace {{PARA}} with paragraph breaks
    local function replacePara(text)
      return text:gsub("%s*{{PARA}}%s*", "\n\n")
    end

    -- Replace list tokens
    local function replaceLists(text)
      text = text:gsub("{{LIST}}", "")
      text = text:gsub("%s*{{LI}}%s*", "\n- ")
      text = text:gsub("{{NLIST}}", "")
      local n = 1
      text = text:gsub("%s*{{NLI}}%s*", function()
        local result = "\n" .. n .. ". "
        n = n + 1
        return result
      end)
      return text
    end

    local summary = replacePara(replaceLists(summary_raw))
    local resolution = replacePara(replaceLists(resolution_raw))

    -- Load the template from same directory as this Lua file
    local script_dir = debug.getinfo(1, "S").source:match("@?(.*/)")
    local template_path = script_dir .. "snippets/request_template.qmd"

    local file = io.open(template_path, "r")
    if not file then
      error("Could not open template file: " .. template_path)
    end

    local template = file:read("*all")
    file:close()

    -- Replace placeholders
    template = template:gsub("{TAG}", tag)
    template = template:gsub("{DATE}", date)
    template = template:gsub("{SOURCE}", source)
    template = template:gsub("{REQUESTED_BY}", requested_by)
    template = template:gsub("{SUMMARY}", summary)
    template = template:gsub("{RESOLUTION}", resolution)

    -- Parse the final string as Markdown
    return pandoc.read(template, "markdown").blocks
  end
}
