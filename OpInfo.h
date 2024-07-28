/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

namespace lucid {

enum class Assoc { None, Left, Right };
    
class OpInfo {
  public:
    OpInfo() { }
    
    // assign says this is an assignmentOperator, opAssign says it also has a binary op
    OpInfo(Operator oper, uint8_t prec, Assoc assoc, Op opcodeS, Op opcodeU, Type resultType)
        : _oper(oper)
        , _prec(prec)
        , _assoc(assoc)
        , _opcodeS(opcodeS)
        , _opcodeU(opcodeU)
        , _resultType(resultType)
    {
    }

    Operator oper() const { return _oper; }
    Op opcodeS() const { return _opcodeS; }
    Op opcodeU() const { return _opcodeU; }
    uint8_t prec() const { return _prec; }
    Assoc assoc() const { return _assoc; }
    Type resultType() const { return _resultType; }

  private:
    Operator _oper;
    uint8_t _prec;
    Assoc _assoc;
    Op _opcodeS;
    Op _opcodeU;
    Type _resultType; // if Type::None then result is the left (or only) operand
};

static std::vector<OpInfo> info {
    { Operator::Equal , 1 , Assoc::Right, Op::NOP  , Op::NOP  , Type::None    }, // OpNode knows if this is an assignment.
    { Operator::AddSto, 1 , Assoc::Right, Op::ADD  , Op::ADD  , Type::None    }, // so all the ...Sto operators (including
    { Operator::SubSto, 1 , Assoc::Right, Op::SUB  , Op::SUB  , Type::None    }, // Equal) just need the binary operation
    { Operator::MulSto, 1 , Assoc::Right, Op::IMUL , Op::UMUL , Type::None    }, // they need to perform. For equal there
    { Operator::DivSto, 1 , Assoc::Right, Op::IDIV , Op::UDIV , Type::None    }, // is no additional operation, so it is
    { Operator::AndSto, 1 , Assoc::Right, Op::AND  , Op::AND  , Type::None    }, //
    { Operator::OrSto , 1 , Assoc::Right, Op::OR   , Op::OR   , Type::None    }, //
    { Operator::XorSto, 1 , Assoc::Right, Op::XOR  , Op::XOR  , Type::None    }, //
    { Operator::LOr   , 2 , Assoc::Left , Op::LOR  , Op::LOR  , Type::UInt8   }, // Logical AND, OR and NOT don't have opcodes.
    { Operator::LAnd  , 3 , Assoc::Left , Op::LAND , Op::LAND , Type::UInt8   }, // They are short-circuited at compile time.
    { Operator::Or    , 4 , Assoc::Left , Op::OR   , Op::OR   , Type::None    },
    { Operator::Xor   , 5 , Assoc::Left , Op::XOR  , Op::XOR  , Type::None    },
    { Operator::And   , 6 , Assoc::Left , Op::AND  , Op::AND  , Type::None    },
    { Operator::EQ    , 7 , Assoc::Left , Op::EQ   , Op::EQ   , Type::UInt8   }, // FIXME: How do the branch opcodes relate to these operators?
    { Operator::NE    , 7 , Assoc::Left , Op::NE   , Op::NE   , Type::UInt8   },
    { Operator::LT    , 8 , Assoc::Left , Op::LT   , Op::LO   , Type::UInt8   },
    { Operator::GT    , 8 , Assoc::Left , Op::GT   , Op::HI   , Type::UInt8   },
    { Operator::GE    , 8 , Assoc::Left , Op::GE   , Op::HS   , Type::UInt8   },
    { Operator::LE    , 8 , Assoc::Left , Op::LE   , Op::LS   , Type::UInt8   },
    { Operator::Plus  , 10, Assoc::Left , Op::ADD  , Op::ADD  , Type::None    },
    { Operator::Minus , 10, Assoc::Left , Op::SUB  , Op::SUB  , Type::None    },
    { Operator::Mul   , 11, Assoc::Left , Op::IMUL , Op::UMUL , Type::None    },
    { Operator::Div   , 11, Assoc::Left , Op::IDIV , Op::UDIV , Type::None    },
};

inline bool findOpInfo(Operator oper, OpInfo& opInfo)
{
    auto it = find_if(info.begin(), info.end(),
                    [oper](const OpInfo opInfo) { return opInfo.oper() == oper; });
    if (it != info.end()) {
        opInfo = *it;
        return true;
    }
    return false;
}
    

}
