local function get_prefix()
	local dir_sep = package.config:sub(1,1)
	local script_dir = debug.getinfo(2, "S").source:match('.(.*' .. dir_sep .. ')')
	if script_dir == nil then
		return '.' .. dir_sep
	else
		return script_dir
	end
end

local prefix = get_prefix()
local config_filename = prefix .. 'config'
local default_problem =
	{ board =
		{ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0x60, 0x70, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xd0, 0xd0, 0xff, 0xff, 0xff
		, 0xff, 0xff, 0xff, 0xd0, 0xd0, 0xff, 0xff, 0xff
		}
	, bcd_virus_count = 0x99
	, pill_left = 0
	, pill_right = 0
	, speed_adjustment = 0
	, goal_x = 3
	, goal_y = 3
	, goal_orientations = { 0, 2 }
	}
local PROBLEMS = { default_problem }
local CURRENT_PROBLEM = 1

-- Where you would write
--     x <- act
--     foo
-- in Haskell, write
--     bind(act, function(x)
--     foo
-- in Lua. Where you would write
--     unless p (throwError e)
-- in Haskell, write
--     bind({p, e}, function()
-- in Lua. Only wrinkle: however many times you bind in your "do block", you
-- need that many copies of end) afterwards.
local function bind(act, f)
	if act[1] then return f(act[2]) else return act end
end

local function forM(kvs, f)
	local RESULT = {}
	for k, v in pairs(kvs) do
		local fv = f(v)
		if fv[1] then RESULT[k] = fv[2] else return fv end
	end
	return {true, RESULT}
end

local function to_bcd(n)
	return n % 10 + 8 * (n - n % 10) / 5
end

local function from_bcd(n)
	return n % 16 + 5 * (n - n % 16) / 8
end

local function parse_file(filename)
	local HANDLE, err_description = io.open(filename, "r")
	if HANDLE == nil then
		return {false, ("Could not open file for reading (%s)"):format(err_description)}
	end

	local RESULT = {}
	local SUCCESS = true
	local LINE_NUMBER = 1

	while SUCCESS do
		local line = HANDLE:read("*line")
		if line == nil then break end

		if line:find("^%s*$") == nil and line:find("^%s*#.*$") == nil then
			k, v = line:match("^%s*(.-)%s*=%s*(.-)%s*$")

			if k == nil then
				SUCCESS = false
				RESULT = 'invalid syntax on line ' .. LINE_NUMBER
			elseif RESULT[k] == nil then
				RESULT[k] = { v }
			else
				table.insert(RESULT[k],v)
			end

			LINE_NUMBER = LINE_NUMBER+1
		end
	end

	HANDLE:close()
	return {SUCCESS, RESULT}
end

local function check_version(kvs, expected_version)
	local version_list = kvs['version'] or {}
	return bind({#version_list == 1, ('Expected exactly one version (saw %d)'):format(#version_list)}, function()
	return bind({version_list[1] == expected_version, ('Expected version %s (saw %s)'):format(expected_version, version_list[1])}, function()
	return {true, kvs}
	end) end)
end

local function parse_versioned_file(filename, version)
	return bind(parse_file(filename), function(kvs)
	return check_version(kvs, version)
	end)
end

local color_lower_nibble =
	{   y   = 0x00
	,   Y   = 0x00
	,   r   = 0x01
	,   R   = 0x01
	,   b   = 0x02
	,   B   = 0x02
	}
local shape_upper_nibble =
	{ ['^'] = 0x40
	,   v   = 0x50
	,   V   = 0x50
	, ['<'] = 0x60
	, ['>'] = 0x70
	,   o   = 0x80
	,   x   = 0xd0
	}

local function parse_cell(s)
	if s == '--' then
		return {true, 0xff}
	elseif s:find('[BbRrYy][xo><Vv^]') then
		local byte = color_lower_nibble[s:sub(1,1)] + shape_upper_nibble[s:sub(2,2)]
		return {true, byte}
	elseif s:find('%x%x') then
		return {true, tonumber('0x' .. s)}
	else
		return {false, ('Expected a cell, namely --, two hex digits, or a color in r, y, b and a shape in x, o, ^, v, <, > (saw %s)'):format(s)}
	end
end

local function parse_problem_row(row)
	local I = 1
	return forM({1,2,3,4,5,6,7,8}, function(j)
		I = row:find('%S%S',I)
		return bind({I ~= nil, ('Expected 8 cells (saw %d in %s)'):format(j-1, row)}, function()
		return bind({j < 8 or I+1 == row:len(), ('Expected 8 cells (saw more in %s)'):format(row)}, function()
		I = I+2
		return parse_cell(row:sub(I-2,I-1))
		end) end)
	end)
end

local function parse_number(s, k, lo, hi)
	local n = tonumber(s)
	return bind({n ~= nil and lo <= n and n <= hi and math.floor(n) == n, ('Expected an integer in the range %d-%d in %s (saw %s)'):format(lo, hi, k, s)}, function()
	return {true, n}
	end)
end

local function parse_single_number(kvs, k, def, lo, hi)
	local v = kvs[k] or def
	return bind({#v == 1, ('Expected exactly one %s (saw %d)'):format(k, #v)}, function()
	return parse_number(v[1], k, lo, hi)
	end)
end

local function parse_pill(vs)
	return bind({#vs == 1, ('Expected exactly one pill (saw %d)'):format(#vs)}, function()
	local v = vs[1]
	return bind({v:find('^[rRyYbB][rRyYbB]$'), ('Expected two colors drawn from r, y, b (saw %s)'):format(v)}, function()
	return {true, {color_lower_nibble[v:sub(1,1)], color_lower_nibble[v:sub(2,2)]}}
	end) end)
end

local function parse_orientations(vs)
	return bind({#vs >= 1, ('Expected at least one goal orientation (saw %d)'):format(#vs)}, function()
	return forM(vs, function(v) return parse_number(v, 'goal orientation', 0, 3) end)
	end)
end

local function concat(xss)
	local YS = {}
	local I = 1
	for _, xs in pairs(xss) do
		for _, x in pairs(xs) do
			YS[I] = x
			I = I + 1
		end
	end
	return YS
end

local function parse_problem(filename)
	return bind(parse_versioned_file(filename, '1'), function(kvs)
	local row_count = #(kvs.row or {})
	return bind({row_count, ('Expected exactly 16 rows (saw %d)'):format(row_count)}, function()
	return bind(forM(kvs.row, parse_problem_row), function(rows)
	return bind(parse_single_number(kvs, 'viruses', {'99'}, 1, 99), function(virus_count)
	return bind(parse_pill(kvs.pill or {'ry'}), function(pill)
	return bind(parse_single_number(kvs, 'speed', {'0'}, 0, 49), function(speed_adjustment)
	return bind(parse_single_number(kvs, 'goal x', {}, 0, 7), function(goal_x)
	return bind(parse_single_number(kvs, 'goal y', {}, 0, 15), function(goal_y)
	return bind(parse_orientations(kvs['goal orientation'] or {}), function(goal_orientations)
	return {true,
		{ board = concat(rows)
		, bcd_virus_count = to_bcd(virus_count)
		, pill_left = pill[1]
		, pill_right = pill[2]
		, speed_adjustment = speed_adjustment
		, goal_x = goal_x
		, goal_y = goal_y
		, goal_orientations = goal_orientations
		}}
	end) end) end) end) end) end) end) end) end)
end

local function parse_config(filename)
	local PROBLEMS = {}
	return bind(parse_versioned_file(filename, '1'), function(kvs)

	for _, problem_suffix in pairs(kvs.problem or {}) do
		local problem_filename = prefix .. problem_suffix
		local problem = parse_problem(problem_filename)
		if problem[1] then
			table.insert(PROBLEMS, problem[2])
		else
			print('Skipping invalid problem description in ' .. problem_filename .. ':\n' .. problem[2])
		end
	end

	return {true, { problems = PROBLEMS }}
	end)
end

local function reload_config()
	local config = parse_config(config_filename)

	PROBLEMS = {}
	if config[1] then
		PROBLEMS = config[2].problems
	else
		print('Skipping invalid config file ' .. config_filename .. ':\n' .. config[2])
	end

	if #PROBLEMS == 0 then
		print('No valid problems specified in ' .. config_filename .. '.\nUsing a hardcoded default problem as a fallback.')
		table.insert(PROBLEMS, default_problem)
	end
end

local initial_state = 0 -- waiting for the user to start the game
local virus_generation_state = 1 -- the game is picking random viruses, let's politely wait for it to finish
local ready_state = 2 -- our turn: time modify the board and pill lookahead however we want
local awaiting_control_state = 3 -- we have set up the board and pill lookahead appropriately, now we're waiting for the pill animation to complete and the pill to appear at the top of the board
local under_control_state = 4 -- the player is doing maneuvers
local maybe_maybe_locked_state = 5 -- the pill is about to drop one row
local maybe_locked_state = 6 -- the pill just dropped one row, and may be either locked about to drop another row
local locked_state = 7 -- the pill locked two frames ago
local CURRENT_STATE = initial_state
local MANEUVER_START_FRAME

local function initial_action()
	local mode = memory.readbyte(0x46)
	local player_count = memory.readbyte(0x727)
	local level = math.min(memory.readbyte(0x316), 20)
	local bcd_virus_count = memory.readbyte(0x324)
	local max_virus_count = (level+1)*4
	-- virus_count < max_virus_count is important: we want to wait until
	-- they're equal, but sometimes they are spuriously equal when the number
	-- of viruses is left over unchanged from a previous game
	if mode == 8 and player_count == 1 and bcd_virus_count < to_bcd(max_virus_count) then
		CURRENT_STATE = virus_generation_state
	end
end

local function virus_generation_action()
	local level = math.min(memory.readbyte(0x316), 20)
	local bcd_virus_count = memory.readbyte(0x324)
	local max_virus_count = (level+1)*4
	if bcd_virus_count == to_bcd(max_virus_count) then
		CURRENT_STATE = ready_state
	end
end

local function ready_action()
	local problem = PROBLEMS[CURRENT_PROBLEM]
	for i, v in pairs(problem.board) do
		memory.writebyte(0x400 + i - 1, v)
	end
	memory.writebyte(0x324, problem.bcd_virus_count)
	for i = 0, 127 do
		memory.writebyte(0x780 + i, 3*problem.pill_left + problem.pill_right)
	end
	memory.writebyte(0x31a, problem.pill_left)
	memory.writebyte(0x31b, problem.pill_right)
	memory.writebyte(0x30a, problem.speed_adjustment)
	CURRENT_STATE = awaiting_control_state
end

local function awaiting_control_action()
	local drop = memory.readbyte(0x312)
	if drop > 0 then
		MANEUVER_START_FRAME = emu.framecount()
		CURRENT_STATE = under_control_state
	end
end

local function under_control_action()
	local drop = memory.readbyte(0x312)
	if drop == 0 then
		CURRENT_STATE = maybe_maybe_locked_state
	end
end

local function maybe_maybe_locked_action()
	local drop = memory.readbyte(0x312)
	if drop == 0 then
		CURRENT_STATE = maybe_locked_state
	else
		CURRENT_STATE = under_control_state
	end
end

local function maybe_locked_action()
	local drop = memory.readbyte(0x312)
	if drop == 0 then
		CURRENT_STATE = locked_state
	else
		CURRENT_STATE = under_control_state
	end
end

local function locked_action()
	local problem = PROBLEMS[CURRENT_PROBLEM]
	local pill_x = memory.readbyte(0x305)
	local pill_y = memory.readbyte(0x306)
	local pill_orientation = memory.readbyte(0x325)
	-- This is not quite right; it could be off by one frame if a pill
	-- naturally drops one row, then gets forced to drop a row by the player on
	-- the very next frame, and the forced drop locks it. Currently, since we
	-- set the drop counter bonus index to 0 (and so natural drops always
	-- happen after an even number of frames), this can only occur if all of
	-- the following happen:
	--
	-- * The pill comes under player control on an odd frame.
	-- * The player lets it drop naturally until the second-to-last row before
	--   it locks.
	-- * On the exact frame that the pill transitions from the second-to-last
	--   row to the last row, the player presses down.
	--
	-- This combination seems very uncommon to me, especially given that I
	-- expect the way most people will use this tool is to hold down as much as
	-- they can. So we'll just report a one-frame-incorrect answer in the
	-- vanishingly unlikely case that all the conditions above are met.
	local maneuver_frames = emu.framecount() - 3 - MANEUVER_START_FRAME + 1

	local SUCCESS = false
	for _, goal_orientation in pairs(problem.goal_orientations) do
		SUCCESS = SUCCESS or pill_orientation == goal_orientation
	end
	SUCCESS = SUCCESS and pill_x == problem.goal_x and pill_y == problem.goal_y

	print('maneuver ' .. (SUCCESS and 'completed' or 'failed') .. ' in ' .. maneuver_frames .. ' frames')
	CURRENT_STATE = ready_state
end

local actions =
	{ [initial_state] = initial_action
	, [virus_generation_state] = virus_generation_action
	, [ready_state] = ready_action
	, [awaiting_control_state] = awaiting_control_action
	, [under_control_state] = under_control_action
	, [maybe_maybe_locked_state] = maybe_maybe_locked_action
	, [maybe_locked_state] = maybe_locked_action
	, [locked_state] = locked_action
	}

reload_config()
emu.registerbefore(function() actions[CURRENT_STATE]() end)
