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

#pragma once

#include <libyul/Dialect.h>

#include <libyul/backends/evm/AbstractAssembly.h>
#include <libyul/ASTForward.h>
#include <liblangutil/EVMVersion.h>

#include <map>
#include <set>

namespace solidity::yul
{

struct FunctionCall;
struct Object;

/**
 * Context used during code generation.
 */
struct BuiltinContext
{
	Object const* currentObject = nullptr;
	/// Mapping from named objects to abstract assembly sub IDs.
	std::map<std::string, AbstractAssembly::SubID> subIDs;
};

struct BuiltinFunctionForEVM: public BuiltinFunction
{
	std::optional<evmasm::Instruction> instruction;
	/// Function to generate code for the given function call and append it to the abstract
	/// assembly. Expects all non-literal arguments of the call to be on stack in reverse order
	/// (i.e. right-most argument pushed first).
	/// Expects the caller to set the source location.
	std::function<void(FunctionCall const&, AbstractAssembly&, BuiltinContext&)> generateCode;
};


/**
 * Yul dialect for EVM as a backend.
 * The main difference is that the builtin functions take an AbstractAssembly for the
 * code generation.
 */
struct EVMDialect: public Dialect
{
	struct Handles
	{
		std::optional<BuiltinHandle> add;
		std::optional<BuiltinHandle> exp;
		std::optional<BuiltinHandle> mul;
		std::optional<BuiltinHandle> not_;
		std::optional<BuiltinHandle> shl;
		std::optional<BuiltinHandle> sub;
	};
	/// Constructor, should only be used internally. Use the factory functions below.
	EVMDialect(langutil::EVMVersion _evmVersion, bool _objectAccess);

	/// @returns the builtin function of the given name or a nullptr if it is not a builtin function.
	std::optional<BuiltinHandle> builtin(std::string_view _name) const override;
	std::optional<VerbatimHandle> verbatim(std::string_view _name) const override;

	BuiltinFunctionForEVM const& builtinFunction(BuiltinHandle const& handle) const override;
	BuiltinFunctionForEVM const& verbatimFunction(VerbatimHandle const&) const override;
	/// @returns true if the identifier is reserved. This includes the builtins too.
	bool reservedIdentifier(std::string_view _name) const override;

	std::optional<BuiltinHandle> discardFunction() const override { return m_discardFunction; }
	std::optional<BuiltinHandle> equalityFunction() const override { return m_equalityFunction; }
	std::optional<BuiltinHandle> booleanNegationFunction() const override { return m_booleanNegationFunction; }
	std::optional<BuiltinHandle> memoryStoreFunction() const override { return m_memoryStoreFunction; }
	std::optional<BuiltinHandle> memoryLoadFunction() const override { return m_memoryLoadFunction; }
	std::optional<BuiltinHandle> storageStoreFunction() const override { return m_storageStoreFunction; }
	std::optional<BuiltinHandle> storageLoadFunction() const override { return m_storageLoadFunction; }
	std::optional<BuiltinHandle> hashFunction() const override { return m_hashFunction; }

	static EVMDialect const& strictAssemblyForEVM(langutil::EVMVersion _version);
	static EVMDialect const& strictAssemblyForEVMObjects(langutil::EVMVersion _version);

	langutil::EVMVersion evmVersion() const { return m_evmVersion; }

	bool providesObjectAccess() const { return m_objectAccess; }

	Handles const& handles() const { return m_handles; }

	static SideEffects sideEffectsOfInstruction(evmasm::Instruction _instruction);

	std::map<std::pair<size_t, size_t>, BuiltinFunctionForEVM> const& verbatimFunctions() const { return m_verbatimFunctions; }

protected:
	VerbatimHandle verbatimFunction(size_t _arguments, size_t _returnVariables) const;

	bool const m_objectAccess;
	langutil::EVMVersion const m_evmVersion;
	std::vector<std::optional<BuiltinFunctionForEVM>> m_functions;
	std::map<std::pair<size_t, size_t>, BuiltinFunctionForEVM> mutable m_verbatimFunctions;
	std::set<std::string, std::less<>> m_reserved;

	Handles m_handles{};
	std::optional<BuiltinHandle> m_discardFunction;
	std::optional<BuiltinHandle> m_equalityFunction;
	std::optional<BuiltinHandle> m_booleanNegationFunction;
	std::optional<BuiltinHandle> m_memoryStoreFunction;
	std::optional<BuiltinHandle> m_memoryLoadFunction;
	std::optional<BuiltinHandle> m_storageStoreFunction;
	std::optional<BuiltinHandle> m_storageLoadFunction;
	std::optional<BuiltinHandle> m_hashFunction;
};

}
