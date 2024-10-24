/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "AST.h"

#include <assert.h>
#include <map>

using namespace clvr;

ASTPtr
TypeCastNode::castIfNeeded(ASTPtr& node, Type neededType)
{
    // We can only cast scalar nodes and pointers
    if ((!isEnum(node->type()) && !isScalar(node->type()) && !node->isPointer()) || !isScalar(neededType)) {
        return node;
    }
    
    // If types don't match, add a cast operator
    assert(neededType != Type::None && node->type() != Type::None);
    if (node->type() != neededType) {
        // If node is a constant just change its type
        if (node->astNodeType() == ASTNodeType::Constant) {
            ConstantNode* constNode = reinterpret_cast<ConstantNode*>(node.get());
            if (neededType == Type::Float) {
                // Value must be an unsigned int
                constNode->toFloat();
            } else if (constNode->type() == Type::Float) {
                // Convert value to int
                constNode->toUInt();
            }
            constNode->setType(neededType);

        } else {
            // If we're casting a pointer into a AddrType, there's nothing to do.
            if (neededType != AddrType || !node->isPointer()) {
                node = std::make_shared<TypeCastNode>(neededType, node);
            }
        }
    }
    
    return node;
}

void
BranchNode::fixup(std::vector<uint8_t>& code, AddrNativeType fixupIndex, AddrNativeType addr, BranchSize& branchSize)
{
    // If branchSize is Long or Unknown we need to emit a 2 byte branch.
    // But if the branch would fit in 1 byte, set branchSize to Short and
    // tell the compiler we need another pass
    if (branchSize != BranchSize::Short) {
        if (addr <= 255) {
            branchSize = BranchSize::Short;
        }
        code[fixupIndex] = addr >> 8;
        code[fixupIndex + 1] = addr;
    } else {
        code[fixupIndex] = addr;
    }
}

void
BranchNode::fixup(std::vector<uint8_t>& code, AddrNativeType addr)
{
    AddrNativeType rel = addr - _fixupIndex - 2;
    if (_branchSize == BranchSize::Short) {
        rel += 1;
    }
    BranchNode::fixup(code, _fixupIndex, rel, _branchSize);
}

void
CaseClause::fixup(std::vector<uint8_t>& code, AddrNativeType addr)
{
    BranchNode::fixup(code, _fixupIndex, addr, _branchSize);
}

//void
//SwitchNode::fixupDefault(std::vector<uint8_t>& code, AddrNativeType index, AddrNativeType addr)
//{
//    BranchNode::fixup(code, index, addr, _defaultBranchSize);
//}
