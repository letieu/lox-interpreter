const std = @import("std");
const Declaration = @import("parse.zig").Declaration;
const VarDecl = @import("parse.zig").VarDecl;
const FunctionDecl = @import("parse.zig").FunctionDecl;
const ClassDecl = @import("parse.zig").ClassDecl;
const Statement = @import("parse.zig").Statement;
const Expr = @import("parse.zig").Expr;

pub const Resolver = struct {
    declarations: []Declaration,
    id_distance: std.AutoHashMap(*Expr, usize),
    scopes: std.ArrayList(std.StringHashMap(bool)),
    alloc: std.mem.Allocator,

    const ResolveError = error{ VariableAlreadyDeclared, ReadInOwnInitializer, InvalidReturn, OutOfMemory };

    pub fn init(declarations: []Declaration, alloc: std.mem.Allocator) Resolver {
        return Resolver{
            .declarations = declarations,
            .id_distance = std.AutoHashMap(*Expr, usize).init(alloc),
            .scopes = std.ArrayList(std.StringHashMap(bool)).init(alloc),
            .alloc = alloc,
        };
    }

    pub fn resolve(self: *Resolver) !std.AutoHashMap(*Expr, usize) {
        try self.beginScope();
        for (self.declarations) |*declaration| {
            try self.resolveDeclaration(declaration, false);
        }
        try self.endScope();
        return self.id_distance;
    }

    fn resolveDeclaration(self: *Resolver, declaration: *const Declaration, is_in_function: bool) ResolveError!void {
        switch (declaration.*) {
            .var_decl => |*var_decl| try self.resolveVarDeclaration(var_decl),
            .function_decl => |*func_decl| try self.resolveFunDeclaration(func_decl),
            .class_decl => |*class_decl| try self.resolveClassDeclaration(class_decl),
            .stmt => |*stmt| try self.resolveStatement(stmt, is_in_function),
        }
    }

    fn resolveClassDeclaration(self: *Resolver, class_decl: *const ClassDecl) !void {
        try self.declare(class_decl.name);
        try self.define(class_decl.name);
    }

    fn resolveFunDeclaration(self: *Resolver, fun_decl: *const FunctionDecl) !void {
        try self.declare(fun_decl.function.name);
        try self.define(fun_decl.function.name);
        try self.beginScope();
        for (fun_decl.function.params) |param| {
            try self.declare(param.lexeme);
            try self.define(param.lexeme);
        }
        for (fun_decl.function.body.declarations) |*decl| {
            try self.resolveDeclaration(decl, true);
        }
        try self.endScope();
    }

    fn resolveVarDeclaration(self: *Resolver, var_decl: *const VarDecl) !void {
        try self.declare(var_decl.name);
        if (var_decl.initializer) |initializer| {
            try self.resolveExpr(initializer);
        }
        try self.define(var_decl.name);
    }

    fn resolveStatement(self: *Resolver, stmt: *const Statement, is_in_fun: bool) !void {
        switch (stmt.*) {
            .print => |print| {
                try self.resolveExpr(print.expr);
            },
            .expression => |expr| {
                try self.resolveExpr(expr.expr);
            },
            .block => |block| {
                try self.beginScope();
                for (block.declarations) |*decl| {
                    try self.resolveDeclaration(decl, false);
                }
                try self.endScope();
            },
            .ifStmt => |if_stmt| {
                try self.resolveExpr(if_stmt.condition);
                try self.resolveStatement(if_stmt.inner, false);
                if (if_stmt.elseStmt) |else_stmt| {
                    try self.resolveStatement(else_stmt, false);
                }
            },
            .while_stmt => |while_stmt| {
                try self.resolveExpr(while_stmt.condition);
                try self.resolveStatement(while_stmt.inner, false);
            },
            .for_stmt => |for_stmt| {
                if (for_stmt.initial) |initial| {
                    try self.resolveDeclaration(initial, false);
                }
            },
            .return_stmt => |return_stmt| {
                if (!is_in_fun) {
                    return ResolveError.InvalidReturn;
                }
                if (return_stmt.expr != null)
                    try self.resolveExpr(return_stmt.expr.?);
            },
        }
    }

    fn resolveExpr(self: *Resolver, expr: *Expr) !void {
        switch (expr.*) {
            .assign => |assign| {
                try self.resolveExpr(assign.left);
                try self.calculateDistance(expr);
            },
            .identifier => |*identifier| {
                if (self.scopes.items.len > 0) {
                    var scope = self.scopes.items[self.scopes.items.len - 1];
                    const declared = scope.get(identifier.token.lexeme) != null;
                    if (declared) {
                        const defined = scope.get(identifier.token.lexeme).?;
                        const is_global = self.scopes.items.len == 1;
                        if (declared and !defined and !is_global) {
                            return error.ReadInOwnInitializer;
                        }
                    }
                }
                try self.calculateDistance(expr);
            },
            .unary => |unary| {
                try self.resolveExpr(unary.right);
            },
            .binary => |binary| {
                try self.resolveExpr(binary.left);
                try self.resolveExpr(binary.right);
            },
            .grouping => |grouping| {
                try self.resolveExpr(grouping.expression);
            },
            .call => |call| {
                try self.resolveExpr(call.callee);
                for (call.args) |arg| {
                    try self.resolveExpr(arg);
                }
            },
            .get => |get| {
                try self.resolveExpr(get.object);
            },
            .set => |set| {
                try self.resolveExpr(set.object);
                try self.resolveExpr(set.value);
            },
            .literal => {},
        }
    }

    fn declare(self: *Resolver, name: []const u8) !void {
        if (self.scopes.items.len == 0) return;
        var scope = &self.scopes.items[self.scopes.items.len - 1];
        const is_global = self.scopes.items.len == 1;
        if (scope.contains(name) and !is_global) return error.VariableAlreadyDeclared;

        try scope.put(name, false);
    }

    fn define(self: *Resolver, name: []const u8) !void {
        if (self.scopes.items.len == 0) return;
        var scope = &self.scopes.items[self.scopes.items.len - 1];
        try scope.put(name, true);
    }

    fn beginScope(self: *Resolver) !void {
        const scope = std.StringHashMap(bool).init(self.alloc);
        try self.scopes.append(scope);
    }

    fn endScope(self: *Resolver) !void {
        var poped = self.scopes.pop();
        if (poped != null) poped.?.deinit();
    }

    fn calculateDistance(self: *Resolver, expr: *Expr) !void {
        const name = switch (expr.*) {
            .identifier => |identifier| identifier.token.lexeme,
            .assign => |assign| assign.name.lexeme,
            else => unreachable,
        };

        var i = self.scopes.items.len;

        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].contains(name)) {
                const distance: usize = self.scopes.items.len - i - 1;
                try self.id_distance.put(expr, distance);
                return;
            }
        }
    }
};
