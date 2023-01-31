use std::collections::{HashMap, HashSet};

use crate::cfg::{self, Assignment, BasicBlock, ControlFlowGraph, Expr, Statement, Variable};

pub fn convert(cfg: &mut ControlFlowGraph) {
    add_annotations(cfg);
    rename_variables(cfg);
}

/// Modifies a ControlFlowGraph such that:
///  - basic blocks get an explicit parameter list `params` which
///    corresponds to the free variables used in that basic block,
///    or expected by subsequent ones
///  - edges get an annotation listing the free variables that their
///    originating basic blocks must supply, either by defining them
///    in let statements or by expecting them as parameters
fn add_annotations(cfg: &mut ControlFlowGraph) {
    let mut visited: HashSet<usize> = HashSet::new();
    let mut active_set: Vec<usize> = vec![cfg.exitpoint];

    // visit all nodes in opposite direction of edges
    while !active_set.is_empty() {
        let mut predecessors = vec![];

        // visit members of active set
        for member_index in active_set {
            let mut member = &mut cfg.nodes[member_index];
            let free_variables = member.free_variables();
            let defined_variables = member.defined_variables();

            // find outgoing edges
            // iterate over all of them
            // collect variables used downstream
            let mut downstream_variables = vec![];
            for outgoing_edge in cfg.edges.iter_mut().filter(|e| e.source == member_index) {
                for variable in outgoing_edge.annotations.iter() {
                    if !downstream_variables.contains(variable)
                        && !defined_variables.contains(variable)
                    {
                        downstream_variables.push(variable.clone());
                    }
                }
            }

            // find incoming edges for `member`
            // iterate over all of them
            // and add annotations
            for incoming_edge in cfg
                .edges
                .iter_mut()
                .filter(|e| e.destination == member_index)
            {
                incoming_edge.annotations = vec![];
                for dv in downstream_variables.iter() {
                    incoming_edge.annotations.push(dv.clone());
                }
                for fv in free_variables.iter() {
                    incoming_edge.annotations.push(fv.clone());
                }
            }

            // make implicit parameter explicit
            member.params = free_variables.clone();

            // mark visited
            visited.insert(member_index);

            // collect successors
            for predecessor in cfg
                .edges
                .iter()
                .filter(|e| e.destination == member_index)
                .map(|e| e.source)
            {
                predecessors.push(predecessor);
            }
        }

        // prepare for next iteration:
        // prune successor set and assign to active set
        active_set = predecessors
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
    let mut name_map = HashMap::<String, String>::new();

    let mut counter: usize = 0;
    let mut name_gen = |s: &str| {
        let tmp = format!("{s}_{counter}");
        counter += 1;
        tmp
    };

    // visit all nodes in the graph, in direction of edges
    while !active_set.is_empty() {
        let mut successors = vec![];

        // visit members of active set and rename each occurrence of
        // variables
        for member_index in active_set {
            let member = &mut cfg.nodes[member_index];

            // apply existing rename substitutions
            substitute_names_in_basic_block(member, &name_map);

            // if the basic block has parameters, iterate over them
            for param in member.params.iter_mut() {
                let data_type = param.data_type.to_owned();

                // this name was already used; we need to get a new one
                if names_already_used.contains(&param.name) {
                    let new_name = name_gen(&param.name);
                    let old_name = param.name.clone();
                    name_map.insert(param.name.clone(), new_name.clone());
                    // apply name change to parameter
                    *param = cfg::Variable {
                        name: new_name.clone(),
                        data_type,
                    };
                    // percolate name change to statements
                    for statement in member.statements.iter_mut() {
                        let expression = match statement {
                            cfg::Statement::Let(assignment) => &mut assignment.expr,
                            cfg::Statement::Re(assignment) => &mut assignment.expr,
                            cfg::Statement::Cond(expr) => expr,
                        };
                        rename_expression(expression, old_name.clone(), new_name.clone());
                    }
                    // keep track of this new name
                    names_already_used.push(new_name);
                }
                // first occurence
                else {
                    names_already_used.push(param.name.clone());
                }
            }

            // iterate over all statements
            for statement_index in 0..member.statements.len() {
                let statement = &mut member.statements[statement_index];
                if let Statement::Cond(_) = statement {
                    break;
                }
                let variable = match &statement {
                    Statement::Let(assignment) => &assignment.var,
                    Statement::Re(assignment) => &assignment.var,
                    Statement::Cond(_) => panic!("Cannot get here."),
                };
                let expression = match &statement {
                    Statement::Let(assignment) => &assignment.expr,
                    Statement::Re(assignment) => &assignment.expr,
                    Statement::Cond(_) => panic!("Cannot get here."),
                };
                let var_name = variable.name.clone();

                // this name was already used, get a new one
                if names_already_used.contains(&var_name) {
                    let new_name = name_gen(&var_name);
                    name_map.insert(var_name.clone(), new_name.clone());

                    // map let-statement -> let-statement with new variable name
                    // and reassignment -> let-statement with new variable name
                    *statement = Statement::Let(Assignment {
                        var: Variable {
                            name: new_name.clone(),
                            data_type: variable.data_type.clone(),
                        },
                        expr: expression.clone(),
                    });

                    // apply and percolate name change
                    for let_statement_mut in member.statements.iter_mut().skip(statement_index + 1)
                    {
                        match let_statement_mut {
                            Statement::Let(assignment) | Statement::Re(assignment) => {
                                if assignment.var.name == *var_name {
                                    // expressions following the outer
                                    // let-statement that shadow the
                                    // variable use the new value
                                    break;
                                }
                                rename_expression(
                                    &mut assignment.expr,
                                    var_name.clone(),
                                    new_name.clone(),
                                );
                            }
                            Statement::Cond(expression) => {
                                rename_expression(expression, var_name.clone(), new_name.clone())
                            }
                        };
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

/// Applies the substitution old_name -> new_name to every occurrence
/// of old_name in the expression.
fn rename_expression(expression: &mut Expr, old_name: String, new_name: String) {
    match expression {
        Expr::Var(var) => {
            if var.name == *old_name {
                var.name = new_name;
            }
        }
        Expr::Lit(_) => {}
    };
}

/// Applies all the old_name -> new_name substitutions listed in the
/// dictionary whereever possible.
fn substitute_names_in_expression(expression: &mut Expr, name_map: &HashMap<String, String>) {
    match expression {
        Expr::Var(variable) => {
            if name_map.contains_key(&variable.name) {
                variable.name = name_map.get(&variable.name).unwrap().clone();
            }
        }
        Expr::Lit(_) => {}
    };
}

/// Applies all the old_name -> new_name substitutions listed in the
/// dictionary whereever possible.
fn substitute_names_in_basic_block(member: &mut BasicBlock, name_map: &HashMap<String, String>) {
    for param in member.params.iter_mut() {
        if name_map.contains_key(&param.name) {
            param.name = name_map.get(&param.name).unwrap().clone();
        }
    }

    for statement in member.statements.iter_mut() {
        match statement {
            Statement::Let(assignment) | Statement::Re(assignment) => {
                if name_map.contains_key(&assignment.var.name) {
                    assignment.var.name = name_map.get(&assignment.var.name).unwrap().clone();
                }
                substitute_names_in_expression(&mut assignment.expr, name_map);
            }
            Statement::Cond(expression) => {
                substitute_names_in_expression(expression, name_map);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{DataType, ExprLit},
        cfg::{BasicBlock, Edge, Expr},
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
        //   call block_2
        //
        // block_2:
        //   foo = foz
        // ```
        //
        // should be annotated to:
        //
        // ```
        // block_0(bar, foz):
        //   foo = bar
        //   call block_1(foo)
        //
        // block_1(foo, foz):
        //   baz = foo
        //   foo = 3
        //   call block_2(foz)
        //
        // block_2(foz):
        //   foo = foz
        // ```
        //
        // and renamed to:
        //
        // ```
        // block_0(bar, foz):
        //   foo = bar
        //   call block_1(foo)
        //
        // block_1(foo_0, foz):
        //   baz = foo_0
        //   foo_1 = 3
        //   call block_2(foz)
        //
        // block_2(foz):
        //   foo_2 = foz
        // ```
        let foo_var = Variable {
            name: "foo".to_string(),
            data_type: DataType::U32,
        };
        let foz_var = Variable {
            name: "foz".to_string(),
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
            statements: vec![Statement::Let(Assignment {
                var: foo_var.clone(),
                expr: Expr::Var(bar_var),
            })],
        });

        // block_1
        cfg.nodes.push(BasicBlock {
            index: 1,
            params: vec![foo_var.clone()],
            statements: vec![
                Statement::Let(Assignment {
                    var: baz_var,
                    expr: Expr::Var(foo_var.clone()),
                }),
                Statement::Re(Assignment {
                    var: foo_var.clone(),
                    expr: Expr::Lit(ExprLit::U32(3)),
                }),
            ],
        });

        // block_2
        cfg.nodes.push(BasicBlock {
            index: 1,
            params: vec![],
            statements: vec![Statement::Re(Assignment {
                var: foo_var,
                expr: Expr::Var(foz_var),
            })],
        });

        // edges
        cfg.edges.push(Edge {
            source: 0,
            destination: 1,
            annotations: vec![],
        });
        cfg.edges.push(Edge {
            source: 1,
            destination: 2,
            annotations: vec![],
        });

        cfg.entrypoint = 0;
        cfg.exitpoint = 2;

        cfg
    }

    #[inline]
    fn assert_annotated(cfg: &ControlFlowGraph) {
        for node_index in 0..cfg.nodes.len() {
            let basic_block = &cfg.nodes[node_index];
            let outgoing_edges: Vec<&Edge> = cfg
                .edges
                .iter()
                .filter(|e| e.source == node_index)
                .collect();
            for incoming_edge in cfg.edges.iter().filter(|e| e.destination == node_index) {
                // annotations of incoming edges should either coincide with parameters ...
                let mut annotations_marker = HashSet::<String>::new();
                for a in basic_block.params.iter() {
                    annotations_marker.insert(a.name.clone());
                }
                // ... or should be passed on downstream ...
                for a in outgoing_edges
                    .iter()
                    .flat_map(|edge| edge.annotations.clone())
                {
                    annotations_marker.insert(a.name);
                }
                // so by now all annotations should be marked in the marker.
                assert!(incoming_edge
                    .annotations
                    .iter()
                    .all(|a| annotations_marker.contains(&a.name)));

                // conversely, all parameters should be in the annotations
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
            for statement in basic_block.statements.iter() {
                match statement {
                    Statement::Let(assignment) => {
                        assert!(!variable_names.contains(&assignment.var.name));
                        variable_names.push(assignment.var.name.clone());
                    }
                    Statement::Re(assignment) => {
                        assert!(!variable_names.contains(&assignment.var.name));
                        variable_names.push(assignment.var.name.clone());
                    }
                    Statement::Cond(_expression) => {}
                }
            }
        }
    }

    #[inline]
    fn assert_no_reassignments(cfg: &ControlFlowGraph) {
        for node_index in 0..cfg.nodes.len() {
            let basic_block = &cfg.nodes[node_index];
            for statement in basic_block.statements.iter() {
                assert!(!matches!(statement, Statement::Re(_)));
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
        assert_no_reassignments(&cfg);

        println!("{cfg:#?}");
    }
}
