use std::collections::HashSet;

use crate::cfg::{self, ControlFlowGraph, Variable};

pub fn convert(cfg: &mut ControlFlowGraph) {
    add_annotations(cfg);
    rename_variables(cfg);
}

/// Modifies a ControlFlowGraph such that:
///  - basic blocks get an explicit parameter list `params` which
///    corresponds to the free variables used in that basic block.
///  - edges get an annotation listing the free variables that their
///    originating basic blocks must supply.
fn add_annotations(cfg: &mut ControlFlowGraph) {
    let mut visited: HashSet<usize> = HashSet::new();
    let mut active_set: Vec<usize> = vec![cfg.entrypoint];

    while !active_set.is_empty() {
        let mut successors = vec![];

        // visit members of active set
        for member_index in active_set {
            let mut member = &mut cfg.nodes[member_index];

            // find free variables
            let mut free_variables: Vec<Variable> = vec![];
            let mut defined_variables = vec![];
            for statement in member.statements.iter() {
                match &statement.expr {
                    cfg::Expr::Var(v) => {
                        if !free_variables.contains(&v) && !defined_variables.contains(&v) {
                            free_variables.push(v.to_owned());
                        }
                    }
                    cfg::Expr::Lit(_) => {}
                };
                defined_variables.push(&statement.var);
            }

            // find incoming edges for `member`
            // iterate over all of them
            // and add annotation
            for incoming_edge in cfg
                .edges
                .iter_mut()
                .filter(|e| e.destination == member_index)
            {
                incoming_edge.annotations = free_variables.clone();
            }

            // make implicit parameter explicit
            member.params = free_variables.clone();

            // mark visited
            visited.insert(member_index);

            // collect successors
            for successor in cfg
                .edges
                .iter()
                .filter(|e| e.source == member_index)
                .map(|e| e.destination)
            {
                successors.push(successor);
            }
        }

        // prepare for next iteration:
        // prune successor set and assign to active set
        active_set = successors
            .into_iter()
            .filter(|s| !visited.contains(s))
            .collect();
    }
}

/// Modifies a control flow graph by renaming the variables such that
/// they are all unique
fn rename_variables(cfg: &mut ControlFlowGraph) {
    let mut visited: HashSet<usize> = HashSet::new();
    let mut active_set: Vec<usize> = vec![cfg.entrypoint];
    let mut names_already_used: Vec<String> = vec![];

    let mut counter: usize = 0;
    let mut name_gen = |s: &str| {
        let tmp = format!("{}_{}", s, counter);
        counter += 1;
        tmp
    };

    // iterate over all nodes in the graph, in direction determined
    // by edges
    while !active_set.is_empty() {
        let mut successors = vec![];

        // visit members of active set and rename each occurrence of
        // variables
        for member_index in active_set {
            let member = &mut cfg.nodes[member_index];

            // if the basic block has parameters, iterate over them
            for param in member.params.iter_mut() {
                let data_type = param.data_type.to_owned();

                // this name was already used; we need to get a new one
                if names_already_used.contains(&param.name) {
                    let new_name = name_gen(&param.name);
                    // apply name change
                    *param = cfg::Variable {
                        name: new_name.clone(),
                        data_type,
                    };
                    // percolate name change
                    for let_statement in member.statements.iter_mut() {
                        match &mut let_statement.expr {
                            cfg::Expr::Var(var) => {
                                var.name = new_name.clone();
                            }
                            cfg::Expr::Lit(_) => {}
                        }
                        if let_statement.var.name == *new_name {
                            // expressions following this let-
                            // statement use the new value
                            break;
                        }
                    }
                    // keep track of this new name
                    names_already_used.push(new_name);
                }
                // first occurence
                else {
                    names_already_used.push(param.name.clone());
                }
            }

            // iterate over all let-statements
            for statement_index in 0..member.statements.len() {
                let var_name = member.statements[statement_index].var.name.clone();

                // this name was already used, get a new one
                if names_already_used.contains(&var_name) {
                    let new_name = name_gen(&var_name);

                    // apply and percolate name change
                    for (idx, let_statement_mut) in member
                        .statements
                        .iter_mut()
                        .skip(statement_index)
                        .enumerate()
                    {
                        if let_statement_mut.var.name == *var_name {
                            if idx == 0 {
                                // rename assignee
                                let_statement_mut.var.name = new_name.clone();
                            } else {
                                // expressions following the outer
                                // let-statement that shadow the
                                // variable use the new value
                                break;
                            }
                        }
                        // rename variables in expression
                        match &mut let_statement_mut.expr {
                            cfg::Expr::Var(var) => {
                                if var.name == var_name {
                                    var.name = new_name.clone();
                                }
                            }
                            cfg::Expr::Lit(_) => {}
                        }
                    }

                    // keep track of new name
                    names_already_used.push(new_name.clone());
                }
                // first occurence
                else {
                    names_already_used.push(var_name.clone());
                }
            }

            // mark visited
            visited.insert(member_index);

            // collect successors
            for successor in cfg
                .edges
                .iter()
                .filter(|e| e.source == member_index)
                .map(|e| e.destination)
            {
                successors.push(successor);
            }
        }

        // prepare for next iteration:
        // prune successor set and assign to active set
        active_set = successors
            .into_iter()
            .filter(|successor| !visited.contains(successor))
            .collect();
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{DataType, ExprLit},
        cfg::{BasicBlock, Edge, Expr, LetStmt},
    };

    use super::*;

    fn gen_cfg() -> cfg::ControlFlowGraph {
        // ```
        // block_0:
        //   foo = bar
        //   call block_1
        //
        // block_1:
        //   baz = foo
        //   foo = 3
        // ```
        //
        // should be annotated to:
        //
        // ```
        // block_0(bar):
        //   foo = bar
        //   call block_1(foo)
        //
        // block_1(foo):
        //   baz = foo
        //   foo = 3
        // ```
        //
        // and renamed to:
        //
        // ```
        // block_0(bar):
        //   foo = bar
        //   call block_1(foo)
        //
        // block_1(foo_0):
        //   baz = foo_0
        //   foo_1 = 3
        // ```
        let foo_var = Variable {
            name: "foo".to_string(),
            data_type: DataType::U32,
        };

        let bar_var = Variable {
            name: "bar".to_string(),
            data_type: DataType::U32,
        };

        let baz_var = Variable {
            name: "baz".to_string(),
            data_type: DataType::U32,
        };

        let mut cfg = ControlFlowGraph::default();

        // block_0
        cfg.nodes.push(BasicBlock {
            index: 0,
            params: vec![],
            statements: vec![LetStmt {
                var: foo_var.clone(),
                expr: Expr::Var(bar_var),
            }],
        });

        // block_1
        cfg.nodes.push(BasicBlock {
            index: 1,
            params: vec![foo_var.clone()],
            statements: vec![
                LetStmt {
                    var: baz_var.clone(),
                    expr: Expr::Var(foo_var.clone()),
                },
                LetStmt {
                    var: foo_var.clone(),
                    expr: Expr::Lit(ExprLit::CU32(3)),
                },
            ],
        });

        // edges
        cfg.edges.push(Edge {
            source: 0,
            destination: 1,
            annotations: vec![],
        });

        cfg.exitpoint = 1;

        cfg
    }

    #[inline]
    fn assert_annotated(cfg: &ControlFlowGraph) {
        for node_index in 0..cfg.nodes.len() {
            let basic_block = &cfg.nodes[node_index];
            for incoming_edge in cfg.edges.iter().filter(|e| e.destination == node_index) {
                for annotation in incoming_edge.annotations.iter() {
                    assert!(basic_block.params.contains(annotation));
                }
                for parameter in basic_block.params.iter() {
                    assert!(incoming_edge.annotations.contains(parameter));
                }
            }
        }
    }

    #[inline]
    fn assert_unique_variable_names(cfg: &ControlFlowGraph) {
        let mut variable_names = vec![];
        for node_index in 0..cfg.nodes.len() {
            let basic_block = &cfg.nodes[node_index];
            for param in basic_block.params.iter() {
                assert!(!variable_names.contains(&param.name));
                variable_names.push(param.name.clone());
            }
            for let_statement in basic_block.statements.iter() {
                assert!(!variable_names.contains(&let_statement.var.name));
                variable_names.push(let_statement.var.name.clone());
            }
        }
    }

    #[test]
    fn simple_ssa_test() {
        let mut cfg = gen_cfg();
        add_annotations(&mut cfg);
        assert_annotated(&cfg);

        // println!("{:#?}", cfg);

        rename_variables(&mut cfg);
        assert_unique_variable_names(&cfg);

        println!("{:#?}", cfg);
    }
}
