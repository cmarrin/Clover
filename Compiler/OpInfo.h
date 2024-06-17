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
    OpInfo(Operator oper, uint8_t prec, Assoc assoc, Op opcode)
        : _oper(oper)
        , _prec(prec)
        , _assoc(assoc)
        , _opcode(opcode)
    {
    }
    
    Op opcode() const { return _opcode; }
    uint8_t prec() const { return _prec; }
    Assoc assoc() const { return _assoc; }

  private:
    Operator _oper;
    uint8_t _prec;
    Assoc _assoc;
    Op _opcode;
};

static std::vector<OpInfo> info {
    { Operator::Equal , 1 , Assoc::Right, Op::NOP  }, // OpNode knows if this is an assignment.
    { Operator::AddSto, 1 , Assoc::Right, Op::ADDA }, // so all the ...Sto operators (including
    { Operator::SubSto, 1 , Assoc::Right, Op::SUBA }, // Equal) just need the binary operation
    { Operator::MulSto, 1 , Assoc::Right, Op::MULA }, // they need to perform. For equal there
    { Operator::DivSto, 1 , Assoc::Right, Op::DIVA }, // is no additional operation, so it is
    { Operator::AndSto, 1 , Assoc::Right, Op::ANDA }, //
    { Operator::OrSto , 1 , Assoc::Right, Op::ORA  }, //
    { Operator::XorSto, 1 , Assoc::Right, Op::XORA }, //
    { Operator::LOr   , 2 , Assoc::Left , Op::NOP  }, // Logical AND, OR and NOT don't have opcodes.
    { Operator::LAnd  , 3 , Assoc::Left , Op::NOP  }, // They are short-circuited at compile time.
    { Operator::Or    , 4 , Assoc::Left , Op::ORA  },
    { Operator::Xor   , 5 , Assoc::Left , Op::XORA },
    { Operator::And   , 6 , Assoc::Left , Op::ANDA },
    { Operator::EQ    , 7 , Assoc::Left , Op::NOP  }, // FIXME: How do the branch opcodes relate to these operators?
    { Operator::NE    , 7 , Assoc::Left , Op::NOP  },
    { Operator::LT    , 8 , Assoc::Left , Op::NOP  },
    { Operator::GT    , 8 , Assoc::Left , Op::NOP  },
    { Operator::GE    , 8 , Assoc::Left , Op::NOP  },
    { Operator::LE    , 8 , Assoc::Left , Op::NOP  },
    { Operator::Plus  , 10, Assoc::Left , Op::ADDA },
    { Operator::Minus , 10, Assoc::Left , Op::SUBA },
    { Operator::Mul   , 11, Assoc::Left , Op::MULA },
    { Operator::Div   , 11, Assoc::Left , Op::DIVA },
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
