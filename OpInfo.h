/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

namespace clvr {

class OpInfo {
  public:
    OpInfo() { }
    
    // assign says this is an assignmentOperator, opAssign says it also has a binary op
    OpInfo(Operator oper, uint8_t prec, bool assignment, Op opcodeS, Op opcodeU, Type resultType)
        : _oper(oper)
        , _prec(prec)
        , _assignment(assignment)
        , _opcodeS(opcodeS)
        , _opcodeU(opcodeU)
        , _resultType(resultType)
    {
    }

    Operator oper() const { return _oper; }
    Op opcodeS() const { return _opcodeS; }
    Op opcodeU() const { return _opcodeU; }
    uint8_t prec() const { return _prec; }
    bool assignment() const { return _assignment; }
    Type resultType() const { return _resultType; }

  private:
    Operator _oper;
    uint8_t _prec;
    bool _assignment;
    Op _opcodeS;
    Op _opcodeU;
    Type _resultType; // if Type::None then result is the left (or only) operand
};

static std::vector<OpInfo> info {
    { Operator::Equal , 1 , true  , Op::NOP  , Op::NOP  , Type::None    }, // OpNode knows if this is an assignment.
    { Operator::AddSto, 1 , true  , Op::ADD  , Op::ADD  , Type::None    }, // so all the ...Sto operators (including
    { Operator::SubSto, 1 , true  , Op::SUB  , Op::SUB  , Type::None    }, // Equal) just need the binary operation
    { Operator::MulSto, 1 , true  , Op::IMUL , Op::UMUL , Type::None    }, // they need to perform. For equal there
    { Operator::DivSto, 1 , true  , Op::IDIV , Op::UDIV , Type::None    }, // is no additional operation, so it is
    { Operator::AndSto, 1 , true  , Op::AND1 , Op::AND1 , Type::None    }, //
    { Operator::OrSto , 1 , true  , Op::OR1  , Op::OR1  , Type::None    }, //
    { Operator::XorSto, 1 , true  , Op::XOR1 , Op::XOR1 , Type::None    }, //
    { Operator::LOr   , 2 , false , Op::NOP  , Op::NOP  , Type::UInt8   }, // Logical AND, and OR don't have opcodes.
    { Operator::LAnd  , 3 , false , Op::NOP  , Op::NOP  , Type::UInt8   }, // They are short-circuited at compile time.
    { Operator::Or    , 4 , false , Op::OR1  , Op::OR1  , Type::None    },
    { Operator::Xor   , 5 , false , Op::XOR1 , Op::XOR1 , Type::None    },
    { Operator::And   , 6 , false , Op::AND1 , Op::AND1 , Type::None    },
    { Operator::EQ    , 7 , false , Op::EQ   , Op::EQ   , Type::UInt8   }, // FIXME: How do the branch opcodes relate to these operators?
    { Operator::NE    , 7 , false , Op::NE   , Op::NE   , Type::UInt8   },
    { Operator::LT    , 8 , false , Op::LT   , Op::LO   , Type::UInt8   },
    { Operator::GT    , 8 , false , Op::GT   , Op::HI   , Type::UInt8   },
    { Operator::GE    , 8 , false , Op::GE   , Op::HS   , Type::UInt8   },
    { Operator::LE    , 8 , false , Op::LE   , Op::LS   , Type::UInt8   },
    { Operator::SHR   , 9 , false , Op::ASR1 , Op::SHR1 , Type::UInt8   },
    { Operator::SHL   , 9 , false , Op::SHL1 , Op::SHL1 , Type::UInt8   },
    { Operator::Plus  , 10, false , Op::ADD  , Op::ADD  , Type::None    },
    { Operator::Minus , 10, false , Op::SUB  , Op::SUB  , Type::None    },
    { Operator::Mul   , 11, false , Op::IMUL , Op::UMUL , Type::None    },
    { Operator::Div   , 11, false , Op::IDIV , Op::UDIV , Type::None    },
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
