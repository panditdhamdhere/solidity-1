/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
// SPDX-License-Identifier: GPL-3.0
/**
 * Yul dialects for EVM.
 */

#include <libyul/backends/evm/EVMDialect.h>

#include <libevmasm/Instruction.h>
#include <libevmasm/SemanticInformation.h>
#include <libsolutil/StringUtils.h>
#include <libyul/AST.h>
#include <libyul/AsmAnalysisInfo.h>
#include <libyul/Exceptions.h>
#include <libyul/Object.h>
#include <libyul/Utilities.h>
#include <libyul/backends/evm/AbstractAssembly.h>

#include <regex>
#include <utility>
#include <vector>

using namespace std::string_literals;
using namespace solidity;
using namespace solidity::yul;
using namespace solidity::util;

namespace
{

BuiltinFunctionForEVM createEVMFunction(
	langutil::EVMVersion _evmVersion,
	std::string const& _name,
	evmasm::Instruction _instruction
)
{
	BuiltinFunctionForEVM f;
	evmasm::InstructionInfo info = evmasm::instructionInfo(_instruction, _evmVersion);
	f.name = _name;
	f.numParameters = static_cast<size_t>(info.args);
	f.numReturns = static_cast<size_t>(info.ret);
	f.sideEffects = EVMDialect::sideEffectsOfInstruction(_instruction);
	if (evmasm::SemanticInformation::terminatesControlFlow(_instruction))
	{
		f.controlFlowSideEffects.canContinue = false;
		if (evmasm::SemanticInformation::reverts(_instruction))
		{
			f.controlFlowSideEffects.canTerminate = false;
			f.controlFlowSideEffects.canRevert = true;
		}
		else
		{
			f.controlFlowSideEffects.canTerminate = true;
			f.controlFlowSideEffects.canRevert = false;
		}
	}
	f.isMSize = _instruction == evmasm::Instruction::MSIZE;
	f.literalArguments.clear();
	f.instruction = _instruction;
	f.generateCode = [_instruction](
		FunctionCall const&,
		AbstractAssembly& _assembly,
		BuiltinContext&
	) {
		_assembly.appendInstruction(_instruction);
	};
	return f;
}

BuiltinFunctionForEVM createFunction(
	std::string const& _name,
	size_t _params,
	size_t _returns,
	SideEffects _sideEffects,
	std::vector<std::optional<LiteralKind>> _literalArguments,
	std::function<void(FunctionCall const&, AbstractAssembly&, BuiltinContext&)> _generateCode
)
{
	yulAssert(_literalArguments.size() == _params || _literalArguments.empty(), "");

	BuiltinFunctionForEVM f;
	f.name = _name;
	f.numParameters = _params;
	f.numReturns = _returns;
	f.sideEffects = _sideEffects;
	f.literalArguments = std::move(_literalArguments);
	f.isMSize = false;
	f.instruction = {};
	f.generateCode = std::move(_generateCode);
	return f;
}

std::set<std::string, std::less<>> createReservedIdentifiers(langutil::EVMVersion _evmVersion)
{
	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// basefee for VMs before london.
	auto baseFeeException = [&](evmasm::Instruction _instr) -> bool
	{
		return _instr == evmasm::Instruction::BASEFEE && _evmVersion < langutil::EVMVersion::london();
	};

	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// blobbasefee for VMs before cancun.
	auto blobBaseFeeException = [&](evmasm::Instruction _instr) -> bool
	{
		return _instr == evmasm::Instruction::BLOBBASEFEE && _evmVersion < langutil::EVMVersion::cancun();
	};

	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// mcopy for VMs before london.
	auto mcopyException = [&](evmasm::Instruction _instr) -> bool
	{
		return _instr == evmasm::Instruction::MCOPY && _evmVersion < langutil::EVMVersion::cancun();
	};

	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// prevrandao for VMs before paris.
	auto prevRandaoException = [&](std::string const& _instrName) -> bool
	{
		// Using string comparison as the opcode is the same as for "difficulty"
		return _instrName == "prevrandao" && _evmVersion < langutil::EVMVersion::paris();
	};

	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// blobhash for VMs before cancun.
	auto blobHashException = [&](evmasm::Instruction _instr) -> bool
	{
		return _instr == evmasm::Instruction::BLOBHASH && _evmVersion < langutil::EVMVersion::cancun();
	};
	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the names
	// tstore or tload for VMs before cancun.
	auto transientStorageException = [&](evmasm::Instruction _instr) -> bool
	{
		return
			_evmVersion < langutil::EVMVersion::cancun() &&
			(_instr == evmasm::Instruction::TSTORE || _instr == evmasm::Instruction::TLOAD);
	};

	std::set<std::string, std::less<>> reserved;
	for (auto const& instr: evmasm::c_instructions)
	{
		std::string name = toLower(instr.first);
		if (
			!baseFeeException(instr.second) &&
			!prevRandaoException(name) &&
			!blobHashException(instr.second) &&
			!blobBaseFeeException(instr.second) &&
			!mcopyException(instr.second) &&
			!transientStorageException(instr.second)
		)
			reserved.emplace(name);
	}
	reserved += std::vector<std::string>{
		"linkersymbol",
		"datasize",
		"dataoffset",
		"datacopy",
		"setimmutable",
		"loadimmutable",
	};
	return reserved;
}

std::vector<std::optional<BuiltinFunctionForEVM>> createBuiltins(langutil::EVMVersion _evmVersion, bool _objectAccess)
{

	// Exclude prevrandao as builtin for VMs before paris and difficulty for VMs after paris.
	auto prevRandaoException = [&](std::string const& _instrName) -> bool
	{
		return (_instrName == "prevrandao" && _evmVersion < langutil::EVMVersion::paris()) || (_instrName == "difficulty" && _evmVersion >= langutil::EVMVersion::paris());
	};

	std::vector<std::optional<BuiltinFunctionForEVM>> builtins;
	for (auto const& instr: evmasm::c_instructions)
	{
		std::string name = toLower(instr.first);
		auto const opcode = instr.second;

		if (
			!evmasm::isDupInstruction(opcode) &&
			!evmasm::isSwapInstruction(opcode) &&
			!evmasm::isPushInstruction(opcode) &&
			opcode != evmasm::Instruction::JUMP &&
			opcode != evmasm::Instruction::JUMPI &&
			opcode != evmasm::Instruction::JUMPDEST &&
			_evmVersion.hasOpcode(opcode) &&
			!prevRandaoException(name)
		)
			builtins.emplace_back(createEVMFunction(_evmVersion, name, opcode));
		else
			builtins.emplace_back(std::nullopt);
	}

	auto const createIfObjectAccess = [_objectAccess](
		std::string const& _name,
		size_t _params,
		size_t _returns,
		SideEffects _sideEffects,
		std::vector<std::optional<LiteralKind>> _literalArguments,
		std::function<void(FunctionCall const&, AbstractAssembly&, BuiltinContext&)> _generateCode
	) -> std::optional<BuiltinFunctionForEVM>
	{
		if (!_objectAccess)
			return std::nullopt;
		return createFunction(_name, _params, _returns, _sideEffects, std::move(_literalArguments), std::move(_generateCode));
	};

	builtins.emplace_back(createIfObjectAccess("linkersymbol", 1, 1, SideEffects{}, {LiteralKind::String}, [](
		FunctionCall const& _call,
		AbstractAssembly& _assembly,
		BuiltinContext&
	) {
		yulAssert(_call.arguments.size() == 1, "");
		Expression const& arg = _call.arguments.front();
		_assembly.appendLinkerSymbol(formatLiteral(std::get<Literal>(arg)));
	}));
	builtins.emplace_back(createIfObjectAccess(
		"memoryguard",
		1,
		1,
		SideEffects{},
		{LiteralKind::Number},
		[](
			FunctionCall const& _call,
			AbstractAssembly& _assembly,
			BuiltinContext&
		) {
			yulAssert(_call.arguments.size() == 1, "");
			Literal const* literal = std::get_if<Literal>(&_call.arguments.front());
			yulAssert(literal, "");
			_assembly.appendConstant(literal->value.value());
		})
	);
	builtins.emplace_back(createIfObjectAccess("datasize", 1, 1, SideEffects{}, {LiteralKind::String}, [](
		FunctionCall const& _call,
		AbstractAssembly& _assembly,
		BuiltinContext& _context
	) {
		yulAssert(_context.currentObject, "No object available.");
		yulAssert(_call.arguments.size() == 1, "");
		Expression const& arg = _call.arguments.front();
		YulName const dataName (formatLiteral(std::get<Literal>(arg)));
		if (_context.currentObject->name == dataName.str())
			_assembly.appendAssemblySize();
		else
		{
		std::vector<size_t> subIdPath =
				_context.subIDs.count(dataName.str()) == 0 ?
					_context.currentObject->pathToSubObject(dataName.str()) :
					std::vector<size_t>{_context.subIDs.at(dataName.str())};
			yulAssert(!subIdPath.empty(), "Could not find assembly object <" + dataName.str() + ">.");
			_assembly.appendDataSize(subIdPath);
		}
	}));
	builtins.emplace_back(createIfObjectAccess("dataoffset", 1, 1, SideEffects{}, {LiteralKind::String}, [](
		FunctionCall const& _call,
		AbstractAssembly& _assembly,
		BuiltinContext& _context
	) {
		yulAssert(_context.currentObject, "No object available.");
		yulAssert(_call.arguments.size() == 1, "");
		Expression const& arg = _call.arguments.front();
		YulName const dataName (formatLiteral(std::get<Literal>(arg)));
		if (_context.currentObject->name == dataName.str())
			_assembly.appendConstant(0);
		else
		{
		std::vector<size_t> subIdPath =
				_context.subIDs.count(dataName.str()) == 0 ?
					_context.currentObject->pathToSubObject(dataName.str()) :
					std::vector<size_t>{_context.subIDs.at(dataName.str())};
			yulAssert(!subIdPath.empty(), "Could not find assembly object <" + dataName.str() + ">.");
			_assembly.appendDataOffset(subIdPath);
		}
	}));
	builtins.emplace_back(createIfObjectAccess(
		"datacopy",
		3,
		0,
		SideEffects{
			false,               // movable
			true,                // movableApartFromEffects
			false,               // canBeRemoved
			false,               // canBeRemovedIfNotMSize
			true,                // cannotLoop
			SideEffects::None,   // otherState
			SideEffects::None,   // storage
			SideEffects::Write,  // memory
			SideEffects::None    // transientStorage
		},
		{},
		[](
			FunctionCall const&,
			AbstractAssembly& _assembly,
			BuiltinContext&
		) {
			_assembly.appendInstruction(evmasm::Instruction::CODECOPY);
		}
	));
	builtins.emplace_back(createIfObjectAccess(
		"setimmutable",
		3,
		0,
		SideEffects{
			false,               // movable
			false,               // movableApartFromEffects
			false,               // canBeRemoved
			false,               // canBeRemovedIfNotMSize
			true,                // cannotLoop
			SideEffects::None,   // otherState
			SideEffects::None,   // storage
			SideEffects::Write,  // memory
			SideEffects::None    // transientStorage
		},
		{std::nullopt, LiteralKind::String, std::nullopt},
		[](
			FunctionCall const& _call,
			AbstractAssembly& _assembly,
			BuiltinContext&
		) {
			yulAssert(_call.arguments.size() == 3, "");
			auto const identifier = (formatLiteral(std::get<Literal>(_call.arguments[1])));
			_assembly.appendImmutableAssignment(identifier);
		}
	));
	builtins.emplace_back(createIfObjectAccess(
		"loadimmutable",
		1,
		1,
		SideEffects{},
		{LiteralKind::String},
		[](
			FunctionCall const& _call,
			AbstractAssembly& _assembly,
			BuiltinContext&
		) {
			yulAssert(_call.arguments.size() == 1, "");
			_assembly.appendImmutable(formatLiteral(std::get<Literal>(_call.arguments.front())));
		}
	));
	return builtins;
}

std::regex const& verbatimPattern()
{
	std::regex static const pattern{"verbatim_([1-9]?[0-9])i_([1-9]?[0-9])o"};
	return pattern;
}

}


EVMDialect::EVMDialect(langutil::EVMVersion _evmVersion, bool _objectAccess):
	m_objectAccess(_objectAccess),
	m_evmVersion(_evmVersion),
	m_functions(createBuiltins(_evmVersion, _objectAccess)),
	m_reserved(createReservedIdentifiers(_evmVersion))
{
	auto const assertBuiltin = [this](std::string_view name)
	{
		std::optional<BuiltinHandle> const handle = builtin(name);
		yulAssert(handle.has_value());
		return *handle;
	};
	m_handles.add = assertBuiltin("add");
	m_handles.exp = assertBuiltin("exp");
	m_handles.mul = assertBuiltin("mul");
	m_handles.not_ = assertBuiltin("not");
	m_handles.shl = assertBuiltin("shl");
	m_handles.sub = assertBuiltin("sub");

	m_discardFunction = builtin("pop");
	m_equalityFunction = builtin("eq");
	m_booleanNegationFunction = builtin("iszero");
	m_memoryStoreFunction = builtin("mstore");
	m_memoryLoadFunction = builtin("mload");
	m_storageStoreFunction = builtin("sstore");
	m_storageLoadFunction = builtin("sload");
	m_hashFunction = builtin("keccak256");
}

std::optional<BuiltinHandle> EVMDialect::builtin(std::string_view _name) const
{
	auto it = std::find_if(m_functions.begin(), m_functions.end(), [&_name](auto const& builtin) { return builtin && builtin.value().name == _name; });
	if (it != m_functions.end() && *it && it->value().name == _name)
		return BuiltinHandle{static_cast<size_t>(std::distance(m_functions.begin(), it))};
	return std::nullopt;
}

std::optional<VerbatimHandle> EVMDialect::verbatim(std::string_view _name) const
{
	if (m_objectAccess)
	{
		std::smatch match;
		std::string name(_name);
		if (regex_match(name, match, verbatimPattern()))
			return verbatimFunction(stoul(match[1]), stoul(match[2]));
	}
	return std::nullopt;
}

bool EVMDialect::reservedIdentifier(std::string_view _name) const
{
	if (m_objectAccess)
		if (_name.substr(0, "verbatim"s.size()) == "verbatim")
			return true;
	return m_reserved.count(_name) != 0;
}

EVMDialect const& EVMDialect::strictAssemblyForEVM(langutil::EVMVersion _version)
{
	static std::map<langutil::EVMVersion, std::unique_ptr<EVMDialect const>> dialects;
	static YulStringRepository::ResetCallback callback{[&] { dialects.clear(); }};
	if (!dialects[_version])
		dialects[_version] = std::make_unique<EVMDialect>(_version, false);
	return *dialects[_version];
}

EVMDialect const& EVMDialect::strictAssemblyForEVMObjects(langutil::EVMVersion _version)
{
	static std::map<langutil::EVMVersion, std::unique_ptr<EVMDialect const>> dialects;
	static YulStringRepository::ResetCallback callback{[&] { dialects.clear(); }};
	if (!dialects[_version])
		dialects[_version] = std::make_unique<EVMDialect>(_version, true);
	return *dialects[_version];
}

SideEffects EVMDialect::sideEffectsOfInstruction(evmasm::Instruction _instruction)
{
	auto translate = [](evmasm::SemanticInformation::Effect _e) -> SideEffects::Effect
	{
		return static_cast<SideEffects::Effect>(_e);
	};

	return SideEffects{
		evmasm::SemanticInformation::movable(_instruction),
		evmasm::SemanticInformation::movableApartFromEffects(_instruction),
		evmasm::SemanticInformation::canBeRemoved(_instruction),
		evmasm::SemanticInformation::canBeRemovedIfNoMSize(_instruction),
		true, // cannotLoop
		translate(evmasm::SemanticInformation::otherState(_instruction)),
		translate(evmasm::SemanticInformation::storage(_instruction)),
		translate(evmasm::SemanticInformation::memory(_instruction)),
		translate(evmasm::SemanticInformation::transientStorage(_instruction)),
	};
}

VerbatimHandle EVMDialect::verbatimFunction(size_t _arguments, size_t _returnVariables) const
{
	std::pair<size_t, size_t> key{_arguments, _returnVariables};
	auto [it, emplaced] = m_verbatimFunctions.try_emplace(key);
	if (emplaced)
	{
		BuiltinFunctionForEVM builtinFunction = createFunction(
			"verbatim_" + std::to_string(_arguments) + "i_" + std::to_string(_returnVariables) + "o",
			1 + _arguments,
			_returnVariables,
			SideEffects::worst(),
			std::vector<std::optional<LiteralKind>>{LiteralKind::String} + std::vector<std::optional<LiteralKind>>(_arguments),
			[=](
				FunctionCall const& _call,
				AbstractAssembly& _assembly,
				BuiltinContext&
			) {
				yulAssert(_call.arguments.size() == (1 + _arguments), "");
				Expression const& bytecode = _call.arguments.front();

				_assembly.appendVerbatim(
					asBytes(formatLiteral(std::get<Literal>(bytecode))),
					_arguments,
					_returnVariables
				);
			}
		);
		builtinFunction.isMSize = true;
		it->second = std::move(builtinFunction);
	}
	return VerbatimHandle{_arguments, _returnVariables};
}

BuiltinFunctionForEVM const& EVMDialect::builtinFunction(BuiltinHandle const& handle) const
{
	yulAssert(handle.id < m_functions.size());
	auto const& maybeBuiltin = m_functions[handle.id];
	yulAssert(maybeBuiltin.has_value());
	return *maybeBuiltin;
}

BuiltinFunctionForEVM const& EVMDialect::verbatimFunction(VerbatimHandle const& handle) const
{
	auto it = m_verbatimFunctions.find(std::make_pair(handle.numArgs, handle.numRets));
	yulAssert(it != m_verbatimFunctions.end());
	return it->second;
}
