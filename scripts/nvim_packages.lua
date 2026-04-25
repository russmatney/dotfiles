local function write_to_csv(file_path, data)
	local file = io.open(file_path, "w")
	if not file then
		return error("Could not open file: " .. file_path)
	end

	file:write("Package,GitHub URL\n")
	for _, pkg in ipairs(data) do
		file:write(pkg.name .. "," .. (pkg.url or "N/A") .. "\n")
	end

	file:close()
end

local function extract_github_url(repo)
	if repo then
		return "https://github.com/" .. repo
	end
	return nil
end

local function get_installed_plugins()
	local data = {}
	local plugins = require("lazy").plugins()
	for _, plugin in pairs(plugins) do
		local name = plugin.name or "N/A"
		local url = plugin.url
		table.insert(data, { name = name, url = url })
	end
	return data
end

local function generate_csv()
	local data = get_installed_plugins()
	write_to_csv("installed_packages.csv", data)
end

generate_csv()
