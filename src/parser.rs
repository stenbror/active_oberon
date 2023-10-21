
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Flags {
    Pointer = 1,
    Reference = 2,
    ReadWrite = 3,
    Read = 4
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ActiveOberonNode {
    Module(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Box<ActiveOberonNode> ),
    ImportList(u32, u32, Box<Vec<(Box<ActiveOberonNode>, Option<Box<ActiveOberonNode>>)>>),
    Definition(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Box<Vec<Box<(Box<ActiveOberonNode>, Option<Box<ActiveOberonNode>>)>>>),
    DeclSequence(u32, u32, Box<Vec<Box<ActiveOberonNode>>>, Box<Vec<Box<ActiveOberonNode>>>, Box<Vec<Box<ActiveOberonNode>>>),
    ConstDecl(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    TypeDecl(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    VarDecl(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    ProcDecl(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    ProcHead(u32, u32, Box<ActiveOberonNode>, Option<Flags>, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    SysFlag(u32, u32, Box<ActiveOberonNode>),
    FormalPars(u32, u32, Option<Box<Vec<ActiveOberonNode>>>, Option<Box<ActiveOberonNode>>),
    FPSection(u32, u32, bool, Box<Vec<Box<ActiveOberonNode>>>, Box<ActiveOberonNode>),
    ArrayType(u32, u32, Option<Flags>, Option<Box<Vec<ActiveOberonNode>>>, Box<ActiveOberonNode>),
    RecordType(u32, u32, Option<Flags>, Option<Box<ActiveOberonNode>>, Option<Box<ActiveOberonNode>>),
    PointerType(u32, u32, Option<Flags>, Box<ActiveOberonNode>),
    ObjectType(u32, u32, Option<Flags>, Option<Box<ActiveOberonNode>>, Option<Box<ActiveOberonNode>>, Box<Vec<Box<ActiveOberonNode>>>, Box<ActiveOberonNode>),
    ProcedureType(u32, u32, Option<Flags>, Option<Box<ActiveOberonNode>>),
    FieldDecl(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    FieldList(u32, u32, Box<Vec<Box<ActiveOberonNode>>>),
    StatBlock(u32, u32, Option<Box<ActiveOberonNode>>, Option<Box<ActiveOberonNode>>),
    StatSequence(u32, u32, Box<Vec<Box<ActiveOberonNode>>>),
    Assignment(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Call(u32, u32, Box<ActiveOberonNode>, Option<Box<ActiveOberonNode>>),
    If(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Option<Box<Vec<(Box<ActiveOberonNode>, Box<ActiveOberonNode>)>>>, Option<Box<ActiveOberonNode>>),
    Case(u32, u32, Box<ActiveOberonNode>, Box<Vec<Box<ActiveOberonNode>>>, Option<Box<ActiveOberonNode>>),
    While(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Repeat(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    For(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Option<Box<ActiveOberonNode>>, Box<ActiveOberonNode>),
    Loop(u32, u32, Box<ActiveOberonNode>),
    With(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Exit(u32, u32),
    Return(u32, u32, Option<Box<ActiveOberonNode>>),
    Await(u32, u32, Box<ActiveOberonNode>),
    CaseElement(u32, u32, Box<Vec<Box<ActiveOberonNode>>>, Box<ActiveOberonNode>),
    CaseLabel(u32, u32, Box<ActiveOberonNode>, Option<Box<ActiveOberonNode>>),
    Mul(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Div(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Mod(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Slash(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    And(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    UnaryPlus(u32, u32, Box<ActiveOberonNode>),
    UnaryMinus(u32, u32, Box<ActiveOberonNode>),
    Plus(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Minus(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Or(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Number(u32, u32, String),
    Character(u32, u32, char),
    String(u32, u32, String),
    Nil(u32, u32),
    Invert(u32, u32),
    Set(u32, u32, Option<Box<Vec<Box<ActiveOberonNode>>>>),
    Element(u32, u32, Box<Vec<Box<ActiveOberonNode>>>),
    Equal(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    NotEqual(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Less(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    LessEqual(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Greater(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    GreaterEqual(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    In(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    Is(u32, u32, Box<ActiveOberonNode>, Box<ActiveOberonNode>),
    DotName(u32, u32, String),
    Index(u32, u32, Box<ActiveOberonNode>),
    ExprList(u32, u32, Box<Vec<Box<ActiveOberonNode>>>),
    IdentList(u32, u32, Box<Vec<Box<ActiveOberonNode>>>),
    Qualident(u32, u32, Option<Box<ActiveOberonNode>>, Box<ActiveOberonNode>),
    IdentDef(u32, u32, String, Option<Flags>)
}


pub trait ActiveOberonParserRules {
    fn parse_module(&mut self);
    fn parse_import_list(&mut self);
    fn parse_definition(&mut self);
    fn parse_decl_seq(&mut self);
    fn parse_const_decl(&mut self);
    fn parse_type_decl(&mut self);
    fn parse_var_decl(&mut self);
    fn parse_proc_decl(&mut self);
    fn parse_proc_head(&mut self);
    fn parse_sys_flag(&mut self);
    fn parse_formal_pars(&mut self);
    fn parse_fp_section(&mut self);
    fn parse_type(&mut self);
    fn parse_field_decl(&mut self);
    fn parse_field_list(&mut self);
    fn parse_body(&mut self);
    fn parse_stat_block(&mut self);
    fn parse_stat_seq(&mut self);
    fn parse_statement(&mut self);
    fn parse_case(&mut self);
    fn parse_case_labels(&mut self);
    fn parse_const_expr(&mut self);
    fn parse_expr(&mut self);
    fn parse_simple_expr(&mut self);
    fn parse_term(&mut self);
    fn parse_factor(&mut self);
    fn parse_set(&mut self);
    fn parse_element(&mut self);
    fn parse_relation(&mut self);
    fn parse_mul_op(&mut self);
    fn parse_add_op(&mut self);
    fn parse_designator(&mut self);
    fn parse_qualident(&mut self);
    fn parse_expr_list(&mut self);
    fn parse_ident_list(&mut self);
    fn parse_ident_def(&mut self);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ActiveOberonParser {

}

impl ActiveOberonParserRules for ActiveOberonParser {
    fn parse_module(&mut self) {
        todo!()
    }

    fn parse_import_list(&mut self) {
        todo!()
    }

    fn parse_definition(&mut self) {
        todo!()
    }

    fn parse_decl_seq(&mut self) {
        todo!()
    }

    fn parse_const_decl(&mut self) {
        todo!()
    }

    fn parse_type_decl(&mut self) {
        todo!()
    }

    fn parse_var_decl(&mut self) {
        todo!()
    }

    fn parse_proc_decl(&mut self) {
        todo!()
    }

    fn parse_proc_head(&mut self) {
        todo!()
    }

    fn parse_sys_flag(&mut self) {
        todo!()
    }

    fn parse_formal_pars(&mut self) {
        todo!()
    }

    fn parse_fp_section(&mut self) {
        todo!()
    }

    fn parse_type(&mut self) {
        todo!()
    }

    fn parse_field_decl(&mut self) {
        todo!()
    }

    fn parse_field_list(&mut self) {
        todo!()
    }

    fn parse_body(&mut self) {
        todo!()
    }

    fn parse_stat_block(&mut self) {
        todo!()
    }

    fn parse_stat_seq(&mut self) {
        todo!()
    }

    fn parse_statement(&mut self) {
        todo!()
    }

    fn parse_case(&mut self) {
        todo!()
    }

    fn parse_case_labels(&mut self) {
        todo!()
    }

    fn parse_const_expr(&mut self) {
        todo!()
    }

    fn parse_expr(&mut self) {
        todo!()
    }

    fn parse_simple_expr(&mut self) {
        todo!()
    }

    fn parse_term(&mut self) {
        todo!()
    }

    fn parse_factor(&mut self) {
        todo!()
    }

    fn parse_set(&mut self) {
        todo!()
    }

    fn parse_element(&mut self) {
        todo!()
    }

    fn parse_relation(&mut self) {
        todo!()
    }

    fn parse_mul_op(&mut self) {
        todo!()
    }

    fn parse_add_op(&mut self) {
        todo!()
    }

    fn parse_designator(&mut self) {
        todo!()
    }

    fn parse_qualident(&mut self) {
        todo!()
    }

    fn parse_expr_list(&mut self) {
        todo!()
    }

    fn parse_ident_list(&mut self) {
        todo!()
    }

    fn parse_ident_def(&mut self) {
        todo!()
    }
}