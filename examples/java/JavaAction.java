// This method is part of an automatic generation : do NOT edit-modify
protected void consumeRule(int act) {
  switch ( act ) {
    case 41 : if (DEBUG) { System.out.println("Type ::= PrimitiveType"); }  //$NON-NLS-1$
		    consumePrimitiveType(); 			break;
 
    case 55 : if (DEBUG) { System.out.println("ReferenceType ::= ClassOrInterfaceType"); }  //$NON-NLS-1$
		    consumeReferenceType(); 			break;
 
    case 59 : if (DEBUG) { System.out.println("ClassOrInterface ::= Name"); }  //$NON-NLS-1$
		    consumeClassOrInterfaceName(); 			break;
 
    case 60 : if (DEBUG) { System.out.println("ClassOrInterface ::= GenericType DOT Name"); }  //$NON-NLS-1$
		    consumeClassOrInterface(); 			break;
 
    case 61 : if (DEBUG) { System.out.println("GenericType ::= ClassOrInterface TypeArguments"); }  //$NON-NLS-1$
		    consumeGenericType(); 			break;
 
    case 62 : if (DEBUG) { System.out.println("GenericType ::= ClassOrInterface LESS GREATER"); }  //$NON-NLS-1$
		    consumeGenericTypeWithDiamond(); 			break;
 
    case 63 : if (DEBUG) { System.out.println("ArrayTypeWithTypeArgumentsName ::= GenericType DOT Name"); }  //$NON-NLS-1$
		    consumeArrayTypeWithTypeArgumentsName(); 			break;
 
    case 64 : if (DEBUG) { System.out.println("ArrayType ::= PrimitiveType Dims"); }  //$NON-NLS-1$
		    consumePrimitiveArrayType(); 			break;
 
    case 65 : if (DEBUG) { System.out.println("ArrayType ::= Name Dims"); }  //$NON-NLS-1$
		    consumeNameArrayType(); 			break;
 
    case 66 : if (DEBUG) { System.out.println("ArrayType ::= ArrayTypeWithTypeArgumentsName Dims"); }  //$NON-NLS-1$
		    consumeGenericTypeNameArrayType(); 			break;
 
    case 67 : if (DEBUG) { System.out.println("ArrayType ::= GenericType Dims"); }  //$NON-NLS-1$
		    consumeGenericTypeArrayType(); 			break;
 
    case 69 : if (DEBUG) { System.out.println("Name ::= SimpleName"); }  //$NON-NLS-1$
		    consumeZeroTypeAnnotations(); 			break;
 
    case 74 : if (DEBUG) { System.out.println("UnannotatableName ::= UnannotatableName DOT SimpleName"); }  //$NON-NLS-1$
		    consumeUnannotatableQualifiedName(); 			break;
 
    case 75 : if (DEBUG) { System.out.println("QualifiedName ::= Name DOT SimpleName"); }  //$NON-NLS-1$
		    consumeQualifiedName(false); 			break;
 
    case 76 : if (DEBUG) { System.out.println("QualifiedName ::= Name DOT TypeAnnotations SimpleName"); }  //$NON-NLS-1$
		    consumeQualifiedName(true); 			break;
 
    case 77 : if (DEBUG) { System.out.println("TypeAnnotationsopt ::="); }  //$NON-NLS-1$
		    consumeZeroTypeAnnotations(); 			break;
 
     case 81 : if (DEBUG) { System.out.println("TypeAnnotations0 ::= TypeAnnotations0 TypeAnnotation"); }  //$NON-NLS-1$
		    consumeOneMoreTypeAnnotation(); 			break;
 
     case 82 : if (DEBUG) { System.out.println("TypeAnnotation ::= NormalTypeAnnotation"); }  //$NON-NLS-1$
		    consumeTypeAnnotation(); 			break;
 
     case 83 : if (DEBUG) { System.out.println("TypeAnnotation ::= MarkerTypeAnnotation"); }  //$NON-NLS-1$
		    consumeTypeAnnotation(); 			break;
 
     case 84 : if (DEBUG) { System.out.println("TypeAnnotation ::= SingleMemberTypeAnnotation"); }  //$NON-NLS-1$
		    consumeTypeAnnotation(); 			break;
 
    case 85 : if (DEBUG) { System.out.println("TypeAnnotationName ::= AT308 UnannotatableName"); }  //$NON-NLS-1$
		    consumeAnnotationName() ; 			break;
 
    case 86 : if (DEBUG) { System.out.println("NormalTypeAnnotation ::= TypeAnnotationName LPAREN..."); }  //$NON-NLS-1$
		    consumeNormalAnnotation(true) ; 			break;
 
    case 87 : if (DEBUG) { System.out.println("MarkerTypeAnnotation ::= TypeAnnotationName"); }  //$NON-NLS-1$
		    consumeMarkerAnnotation(true) ; 			break;
 
    case 88 : if (DEBUG) { System.out.println("SingleMemberTypeAnnotation ::= TypeAnnotationName LPAREN"); }  //$NON-NLS-1$
		    consumeSingleMemberAnnotation(true) ; 			break;
 
    case 89 : if (DEBUG) { System.out.println("RejectTypeAnnotations ::="); }  //$NON-NLS-1$
		    consumeNonTypeUseName(); 			break;
 
    case 90 : if (DEBUG) { System.out.println("PushZeroTypeAnnotations ::="); }  //$NON-NLS-1$
		    consumeZeroTypeAnnotations(); 			break;
 
    case 91 : if (DEBUG) { System.out.println("VariableDeclaratorIdOrThis ::= this"); }  //$NON-NLS-1$
		    consumeExplicitThisParameter(false); 			break;
 
    case 92 : if (DEBUG) { System.out.println("VariableDeclaratorIdOrThis ::= UnannotatableName DOT this"); }  //$NON-NLS-1$
		    consumeExplicitThisParameter(true); 			break;
 
    case 93 : if (DEBUG) { System.out.println("VariableDeclaratorIdOrThis ::= VariableDeclaratorId"); }  //$NON-NLS-1$
		    consumeVariableDeclaratorIdParameter(); 			break;
 
    case 94 : if (DEBUG) { System.out.println("CompilationUnit ::= EnterCompilationUnit..."); }  //$NON-NLS-1$
		    consumeCompilationUnit(); 			break;
 
    case 95 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration"); }  //$NON-NLS-1$
		    consumeInternalCompilationUnit(); 			break;
 
    case 96 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration..."); }  //$NON-NLS-1$
		    consumeInternalCompilationUnit(); 			break;
 
    case 97 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration..."); }  //$NON-NLS-1$
		    consumeInternalCompilationUnitWithTypes(); 			break;
 
    case 98 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration..."); }  //$NON-NLS-1$
		    consumeInternalCompilationUnitWithTypes(); 			break;
 
    case 99 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= ImportDeclarations..."); }  //$NON-NLS-1$
		    consumeInternalCompilationUnit(); 			break;
 
    case 100 : if (DEBUG) { System.out.println("InternalCompilationUnit ::="); }  //$NON-NLS-1$
		    consumeEmptyInternalCompilationUnit(); 			break;
 
    case 101 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= ImportDeclarations..."); }  //$NON-NLS-1$
		    consumeInternalCompilationUnitWithModuleDeclaration(); 			break;
 
    case 102 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= ModuleDeclaration"); }  //$NON-NLS-1$
		    consumeInternalCompilationUnitWithModuleDeclaration(); 			break;
 
    case 103 : if (DEBUG) { System.out.println("ModuleDeclaration ::= ModuleHeader ModuleBody"); }  //$NON-NLS-1$
		    consumeModuleDeclaration(); 			break;
 
    case 104 : if (DEBUG) { System.out.println("InternalCompilationUnit ::=..."); }  //$NON-NLS-1$
		    consumeInternalCompilationUnitWithPotentialImplicitlyDeclaredClass(); 			break;
 
    case 105 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= ImportDeclarations..."); }  //$NON-NLS-1$
		    consumeInternalCompilationUnitWithPotentialImplicitlyDeclaredClass(); 			break;
 
    case 106 : if (DEBUG) { System.out.println("ModuleHeader ::= Modifiersopt ModuleModifieropt module"); }  //$NON-NLS-1$
		    consumeModuleHeader(); 			break;
 
    case 108 : if (DEBUG) { System.out.println("ModuleModifieropt ::= ModuleModifier"); }  //$NON-NLS-1$
		    consumeModuleModifiers(); 			break;
 
    case 111 : if (DEBUG) { System.out.println("ModuleStatementsOpt ::="); }  //$NON-NLS-1$
		    consumeEmptyModuleStatementsOpt(); 			break;
 
    case 114 : if (DEBUG) { System.out.println("ModuleStatements ::= ModuleStatements ModuleStatement"); }  //$NON-NLS-1$
		    consumeModuleStatements(); 			break;
 
    case 120 : if (DEBUG) { System.out.println("RequiresStatement ::= SingleRequiresModuleName SEMICOLON"); }  //$NON-NLS-1$
		    consumeRequiresStatement(); 			break;
 
    case 121 : if (DEBUG) { System.out.println("SingleRequiresModuleName ::= requires..."); }  //$NON-NLS-1$
		    consumeSingleRequiresModuleName(); 			break;
 
    case 122 : if (DEBUG) { System.out.println("RequiresModifiersopt ::= RequiresModifiers"); }  //$NON-NLS-1$
		    consumeModifiers(); 			break;
 
    case 123 : if (DEBUG) { System.out.println("RequiresModifiersopt ::="); }  //$NON-NLS-1$
		    consumeDefaultModifiers(); 			break;
 
    case 125 : if (DEBUG) { System.out.println("RequiresModifiers ::= RequiresModifiers RequiresModifier"); }  //$NON-NLS-1$
		    consumeModifiers2(); 			break;
 
    case 128 : if (DEBUG) { System.out.println("ExportsStatement ::= ExportsHeader TargetModuleListopt"); }  //$NON-NLS-1$
		    consumeExportsStatement(); 			break;
 
    case 129 : if (DEBUG) { System.out.println("ExportsHeader ::= exports SinglePkgName"); }  //$NON-NLS-1$
		    consumeExportsHeader(); 			break;
 
    case 131 : if (DEBUG) { System.out.println("TargetModuleListopt ::= to TargetModuleNameList"); }  //$NON-NLS-1$
		    consumeTargetModuleList(); 			break;
 
    case 132 : if (DEBUG) { System.out.println("TargetModuleName ::= UnannotatableName"); }  //$NON-NLS-1$
		    consumeSingleTargetModuleName(); 			break;
 
    case 134 : if (DEBUG) { System.out.println("TargetModuleNameList ::= TargetModuleNameList COMMA..."); }  //$NON-NLS-1$
		    consumeTargetModuleNameList(); 			break;
 
    case 135 : if (DEBUG) { System.out.println("SinglePkgName ::= UnannotatableName"); }  //$NON-NLS-1$
		    consumeSinglePkgName(); 			break;
 
    case 136 : if (DEBUG) { System.out.println("OpensStatement ::= OpensHeader TargetModuleListopt..."); }  //$NON-NLS-1$
		    consumeOpensStatement(); 			break;
 
    case 137 : if (DEBUG) { System.out.println("OpensHeader ::= opens SinglePkgName"); }  //$NON-NLS-1$
		    consumeOpensHeader(); 			break;
 
    case 138 : if (DEBUG) { System.out.println("UsesStatement ::= UsesHeader SEMICOLON"); }  //$NON-NLS-1$
		    consumeUsesStatement(); 			break;
 
    case 139 : if (DEBUG) { System.out.println("UsesHeader ::= uses Name"); }  //$NON-NLS-1$
		    consumeUsesHeader(); 			break;
 
    case 140 : if (DEBUG) { System.out.println("ProvidesStatement ::= ProvidesInterface WithClause..."); }  //$NON-NLS-1$
		    consumeProvidesStatement(); 			break;
 
    case 141 : if (DEBUG) { System.out.println("ProvidesInterface ::= provides Name"); }  //$NON-NLS-1$
		    consumeProvidesInterface(); 			break;
 
    case 142 : if (DEBUG) { System.out.println("ServiceImplName ::= Name"); }  //$NON-NLS-1$
		    consumeSingleServiceImplName(); 			break;
 
    case 144 : if (DEBUG) { System.out.println("ServiceImplNameList ::= ServiceImplNameList COMMA..."); }  //$NON-NLS-1$
		    consumeServiceImplNameList(); 			break;
 
    case 145 : if (DEBUG) { System.out.println("WithClause ::= with ServiceImplNameList"); }  //$NON-NLS-1$
		    consumeWithClause(); 			break;
 
    case 146 : if (DEBUG) { System.out.println("ReduceImports ::="); }  //$NON-NLS-1$
		    consumeReduceImports(); 			break;
 
    case 147 : if (DEBUG) { System.out.println("EnterCompilationUnit ::="); }  //$NON-NLS-1$
		    consumeEnterCompilationUnit(); 			break;
 
    case 170 : if (DEBUG) { System.out.println("CatchHeader ::= catch LPAREN CatchFormalParameter RPAREN"); }  //$NON-NLS-1$
		    consumeCatchHeader(); 			break;
 
    case 172 : if (DEBUG) { System.out.println("ImportDeclarations ::= ImportDeclarations..."); }  //$NON-NLS-1$
		    consumeImportDeclarations(); 			break;
 
    case 174 : if (DEBUG) { System.out.println("TypeDeclarations ::= TypeDeclarations TypeDeclaration"); }  //$NON-NLS-1$
		    consumeTypeDeclarations(); 			break;
 
    case 175 : if (DEBUG) { System.out.println("PackageDeclaration ::= PackageDeclarationName SEMICOLON"); }  //$NON-NLS-1$
		    consumePackageDeclaration(); 			break;
 
    case 176 : if (DEBUG) { System.out.println("PackageDeclarationName ::= Modifiers package..."); }  //$NON-NLS-1$
		    consumePackageDeclarationNameWithModifiers(); 			break;
 
    case 177 : if (DEBUG) { System.out.println("PackageDeclarationName ::= PackageComment package Name"); }  //$NON-NLS-1$
		    consumePackageDeclarationName(); 			break;
 
    case 178 : if (DEBUG) { System.out.println("PackageComment ::="); }  //$NON-NLS-1$
		    consumePackageComment(); 			break;
 
    case 183 : if (DEBUG) { System.out.println("SingleTypeImportDeclaration ::=..."); }  //$NON-NLS-1$
		    consumeImportDeclaration(); 			break;
 
    case 184 : if (DEBUG) { System.out.println("SingleTypeImportDeclarationName ::= import Name..."); }  //$NON-NLS-1$
		    consumeSingleTypeImportDeclarationName(); 			break;
 
    case 185 : if (DEBUG) { System.out.println("TypeImportOnDemandDeclaration ::=..."); }  //$NON-NLS-1$
		    consumeImportDeclaration(); 			break;
 
    case 186 : if (DEBUG) { System.out.println("TypeImportOnDemandDeclarationName ::= import Name DOT..."); }  //$NON-NLS-1$
		    consumeTypeImportOnDemandDeclarationName(); 			break;
 
     case 189 : if (DEBUG) { System.out.println("TypeDeclaration ::= SEMICOLON"); }  //$NON-NLS-1$
		    consumeEmptyTypeDeclaration(); 			break;
 
    case 194 : if (DEBUG) { System.out.println("Modifiers ::= Modifiers Modifier"); }  //$NON-NLS-1$
		    consumeModifiers2(); 			break;
 
    case 208 : if (DEBUG) { System.out.println("Modifier ::= Annotation"); }  //$NON-NLS-1$
		    consumeAnnotationAsModifier(); 			break;
 
    case 209 : if (DEBUG) { System.out.println("ClassDeclaration ::= ClassHeader ClassBody"); }  //$NON-NLS-1$
		    consumeClassDeclaration(); 			break;
 
    case 210 : if (DEBUG) { System.out.println("ClassHeader ::= ClassHeaderName ClassHeaderExtendsopt..."); }  //$NON-NLS-1$
		    consumeClassHeader(); 			break;
 
    case 211 : if (DEBUG) { System.out.println("ClassHeaderName ::= ClassHeaderName1 TypeParameters"); }  //$NON-NLS-1$
		    consumeTypeHeaderNameWithTypeParameters(); 			break;
 
    case 213 : if (DEBUG) { System.out.println("ClassHeaderName1 ::= Modifiersopt class Identifier"); }  //$NON-NLS-1$
		    consumeClassHeaderName1(); 			break;
 
    case 214 : if (DEBUG) { System.out.println("ClassHeaderExtends ::= extends ClassType"); }  //$NON-NLS-1$
		    consumeClassHeaderExtends(); 			break;
 
    case 215 : if (DEBUG) { System.out.println("ClassHeaderImplements ::= implements InterfaceTypeList"); }  //$NON-NLS-1$
		    consumeClassHeaderImplements(); 			break;
 
    case 217 : if (DEBUG) { System.out.println("InterfaceTypeList ::= InterfaceTypeList COMMA..."); }  //$NON-NLS-1$
		    consumeInterfaceTypeList(); 			break;
 
    case 218 : if (DEBUG) { System.out.println("InterfaceType ::= ClassOrInterfaceType"); }  //$NON-NLS-1$
		    consumeInterfaceType(); 			break;
 
    case 221 : if (DEBUG) { System.out.println("ClassBodyDeclarations ::= ClassBodyDeclarations..."); }  //$NON-NLS-1$
		    consumeClassBodyDeclarations(); 			break;
 
    case 226 : if (DEBUG) { System.out.println("ImplicitlyDeclaredClassBodyDeclarations ::=..."); }  //$NON-NLS-1$
		    consumeImplicitlyDeclaredClassBodyDeclarations(); 			break;
 
    case 227 : if (DEBUG) { System.out.println("ClassBodyDeclaration ::= Diet NestedMethod..."); }  //$NON-NLS-1$
		    consumeClassBodyDeclaration(); 			break;
 
    case 228 : if (DEBUG) { System.out.println("Diet ::="); }  //$NON-NLS-1$
		    consumeDiet(); 			break;

    case 229 : if (DEBUG) { System.out.println("Initializer ::= Diet NestedMethod CreateInitializer..."); }  //$NON-NLS-1$
		    consumeClassBodyDeclaration(); 			break;
 
    case 230 : if (DEBUG) { System.out.println("CreateInitializer ::="); }  //$NON-NLS-1$
		    consumeCreateInitializer(); 			break;

    case 238 : if (DEBUG) { System.out.println("ClassMemberDeclaration ::= SEMICOLON"); }  //$NON-NLS-1$
		    consumeEmptyTypeDeclaration(); 			break;

    case 241 : if (DEBUG) { System.out.println("FieldDeclaration ::= Modifiersopt Type..."); }  //$NON-NLS-1$
		    consumeFieldDeclaration(); 			break;
 
    case 243 : if (DEBUG) { System.out.println("VariableDeclarators ::= VariableDeclarators COMMA..."); }  //$NON-NLS-1$
		    consumeVariableDeclarators(); 			break;
 
    case 246 : if (DEBUG) { System.out.println("EnterVariable ::="); }  //$NON-NLS-1$
		    consumeEnterVariable(); 			break;
 
    case 247 : if (DEBUG) { System.out.println("ExitVariableWithInitialization ::="); }  //$NON-NLS-1$
		    consumeExitVariableWithInitialization(); 			break;
 
    case 248 : if (DEBUG) { System.out.println("ExitVariableWithoutInitialization ::="); }  //$NON-NLS-1$
		    consumeExitVariableWithoutInitialization(); 			break;
 
    case 249 : if (DEBUG) { System.out.println("ForceNoDiet ::="); }  //$NON-NLS-1$
		    consumeForceNoDiet(); 			break;
 
    case 250 : if (DEBUG) { System.out.println("RestoreDiet ::="); }  //$NON-NLS-1$
		    consumeRestoreDiet(); 			break;
 
    case 252 : if (DEBUG) { System.out.println("VariableDeclaratorId ::= UNDERSCORE"); }  //$NON-NLS-1$
		    consumeUnnamedVariable(); 			break;
 
    case 256 : if (DEBUG) { System.out.println("MethodDeclaration ::= MethodHeader MethodBody"); }  //$NON-NLS-1$
		    // set to true to consume a method with a body
 consumeMethodDeclaration(true, false); 			break;
 
    case 257 : if (DEBUG) { System.out.println("MethodDeclaration ::= DefaultMethodHeader MethodBody"); }  //$NON-NLS-1$
		    // set to true to consume a method with a body
 consumeMethodDeclaration(true, true); 			break;
 
    case 258 : if (DEBUG) { System.out.println("AbstractMethodDeclaration ::= MethodHeader SEMICOLON"); }  //$NON-NLS-1$
		    // set to false to consume a method without body
 consumeMethodDeclaration(false, false); 			break;
 
    case 259 : if (DEBUG) { System.out.println("MethodHeader ::= MethodHeaderName FormalParameterListopt"); }  //$NON-NLS-1$
		    consumeMethodHeader(); 			break;
 
    case 260 : if (DEBUG) { System.out.println("DefaultMethodHeader ::= DefaultMethodHeaderName..."); }  //$NON-NLS-1$
		    consumeMethodHeader(); 			break;
 
    case 261 : if (DEBUG) { System.out.println("MethodHeaderName ::= Modifiersopt TypeParameters Type..."); }  //$NON-NLS-1$
		    consumeMethodHeaderNameWithTypeParameters(false); 			break;
 
    case 262 : if (DEBUG) { System.out.println("MethodHeaderName ::= Modifiersopt Type Identifier LPAREN"); }  //$NON-NLS-1$
		    consumeMethodHeaderName(false); 			break;
 
    case 263 : if (DEBUG) { System.out.println("DefaultMethodHeaderName ::= ModifiersWithDefault..."); }  //$NON-NLS-1$
		    consumeMethodHeaderNameWithTypeParameters(false); 			break;
 
    case 264 : if (DEBUG) { System.out.println("DefaultMethodHeaderName ::= ModifiersWithDefault Type..."); }  //$NON-NLS-1$
		    consumeMethodHeaderName(false); 			break;
 
    case 265 : if (DEBUG) { System.out.println("ModifiersWithDefault ::= Modifiersopt default..."); }  //$NON-NLS-1$
		    consumePushCombineModifiers(); 			break;
 
    case 266 : if (DEBUG) { System.out.println("MethodHeaderRightParen ::= RPAREN"); }  //$NON-NLS-1$
		    consumeMethodHeaderRightParen(); 			break;
 
    case 267 : if (DEBUG) { System.out.println("MethodHeaderExtendedDims ::= Dimsopt"); }  //$NON-NLS-1$
		    consumeMethodHeaderExtendedDims(); 			break;
 
    case 268 : if (DEBUG) { System.out.println("MethodHeaderThrowsClause ::= throws ClassTypeList"); }  //$NON-NLS-1$
		    consumeMethodHeaderThrowsClause(); 			break;
 
    case 269 : if (DEBUG) { System.out.println("ConstructorHeader ::= ConstructorHeaderName..."); }  //$NON-NLS-1$
		    consumeConstructorHeader(); 			break;
 
    case 270 : if (DEBUG) { System.out.println("ConstructorHeaderName ::= Modifiersopt TypeParameters..."); }  //$NON-NLS-1$
		    consumeConstructorHeaderNameWithTypeParameters(); 			break;
 
    case 271 : if (DEBUG) { System.out.println("ConstructorHeaderName ::= Modifiersopt Identifier LPAREN"); }  //$NON-NLS-1$
		    consumeConstructorHeaderName(); 			break;
 
    case 273 : if (DEBUG) { System.out.println("FormalParameterList ::= FormalParameterList COMMA..."); }  //$NON-NLS-1$
		    consumeFormalParameterList(); 			break;
 
    case 274 : if (DEBUG) { System.out.println("FormalParameter ::= Modifiersopt Type..."); }  //$NON-NLS-1$
		    consumeFormalParameter(false); 			break;
 
    case 275 : if (DEBUG) { System.out.println("FormalParameter ::= Modifiersopt Type..."); }  //$NON-NLS-1$
		    consumeFormalParameter(true); 			break;
 
    case 276 : if (DEBUG) { System.out.println("FormalParameter ::= Modifiersopt Type AT308DOTDOTDOT..."); }  //$NON-NLS-1$
		    consumeFormalParameter(true); 			break;
 
    case 277 : if (DEBUG) { System.out.println("CatchFormalParameter ::= Modifiersopt CatchType..."); }  //$NON-NLS-1$
		    consumeCatchFormalParameter(); 			break;
 
    case 278 : if (DEBUG) { System.out.println("CatchType ::= UnionType"); }  //$NON-NLS-1$
		    consumeCatchType(); 			break;
 
    case 279 : if (DEBUG) { System.out.println("UnionType ::= Type"); }  //$NON-NLS-1$
		    consumeUnionTypeAsClassType(); 			break;
 
    case 280 : if (DEBUG) { System.out.println("UnionType ::= UnionType OR Type"); }  //$NON-NLS-1$
		    consumeUnionType(); 			break;
 
    case 282 : if (DEBUG) { System.out.println("ClassTypeList ::= ClassTypeList COMMA ClassTypeElt"); }  //$NON-NLS-1$
		    consumeClassTypeList(); 			break;
 
    case 283 : if (DEBUG) { System.out.println("ClassTypeElt ::= ClassType"); }  //$NON-NLS-1$
		    consumeClassTypeElt(); 			break;
 
    case 284 : if (DEBUG) { System.out.println("MethodBody ::= NestedMethod LBRACE BlockStatementsopt..."); }  //$NON-NLS-1$
		    consumeMethodBody(); 			break;
 
    case 285 : if (DEBUG) { System.out.println("NestedMethod ::="); }  //$NON-NLS-1$
		    consumeNestedMethod(); 			break;
 
    case 286 : if (DEBUG) { System.out.println("StaticInitializer ::= StaticOnly Block"); }  //$NON-NLS-1$
		    consumeStaticInitializer(); 			break;

    case 287 : if (DEBUG) { System.out.println("StaticOnly ::= static"); }  //$NON-NLS-1$
		    consumeStaticOnly(); 			break;
 
    case 288 : if (DEBUG) { System.out.println("ConstructorDeclaration ::= ConstructorHeader MethodBody"); }  //$NON-NLS-1$
		    consumeConstructorDeclaration() ; 			break;
 
    case 289 : if (DEBUG) { System.out.println("ConstructorDeclaration ::= ConstructorHeader SEMICOLON"); }  //$NON-NLS-1$
		    consumeInvalidConstructorDeclaration() ; 			break;
 
    case 290 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= this LPAREN..."); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocation(0, THIS_CALL); 			break;
 
    case 291 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= OnlyTypeArguments this"); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocationWithTypeArguments(0,THIS_CALL); 			break;
 
    case 292 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= super LPAREN..."); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocation(0,SUPER_CALL); 			break;
 
    case 293 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= OnlyTypeArguments..."); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocationWithTypeArguments(0,SUPER_CALL); 			break;
 
    case 294 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT super..."); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocation(1, SUPER_CALL); 			break;
 
    case 295 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT..."); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocationWithTypeArguments(1, SUPER_CALL); 			break;
 
    case 296 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT super LPAREN"); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocation(2, SUPER_CALL); 			break;
 
    case 297 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT..."); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocationWithTypeArguments(2, SUPER_CALL); 			break;
 
    case 298 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT this..."); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocation(1, THIS_CALL); 			break;
 
    case 299 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT..."); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocationWithTypeArguments(1, THIS_CALL); 			break;
 
    case 300 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT this LPAREN"); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocation(2, THIS_CALL); 			break;
 
    case 301 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT..."); }  //$NON-NLS-1$
		    consumeExplicitConstructorInvocationWithTypeArguments(2, THIS_CALL); 			break;
 
    case 302 : if (DEBUG) { System.out.println("InterfaceDeclaration ::= InterfaceHeader InterfaceBody"); }  //$NON-NLS-1$
		    consumeInterfaceDeclaration(); 			break;
 
    case 303 : if (DEBUG) { System.out.println("InterfaceHeader ::= InterfaceHeaderName..."); }  //$NON-NLS-1$
		    consumeInterfaceHeader(); 			break;
 
    case 304 : if (DEBUG) { System.out.println("InterfaceHeaderName ::= InterfaceHeaderName1..."); }  //$NON-NLS-1$
		    consumeTypeHeaderNameWithTypeParameters(); 			break;
 
    case 306 : if (DEBUG) { System.out.println("InterfaceHeaderName1 ::= Modifiersopt interface..."); }  //$NON-NLS-1$
		    consumeInterfaceHeaderName1(); 			break;
 
    case 307 : if (DEBUG) { System.out.println("InterfaceHeaderExtends ::= extends InterfaceTypeList"); }  //$NON-NLS-1$
		    consumeInterfaceHeaderExtends(); 			break;
 
    case 310 : if (DEBUG) { System.out.println("InterfaceMemberDeclarations ::=..."); }  //$NON-NLS-1$
		    consumeInterfaceMemberDeclarations(); 			break;
 
    case 311 : if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= SEMICOLON"); }  //$NON-NLS-1$
		    consumeEmptyTypeDeclaration(); 			break;
 
    case 313 : if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= DefaultMethodHeader..."); }  //$NON-NLS-1$
		    consumeInterfaceMethodDeclaration(false); 			break;
 
    case 314 : if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= MethodHeader MethodBody"); }  //$NON-NLS-1$
		    consumeInterfaceMethodDeclaration(false); 			break;
 
    case 315 : if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= DefaultMethodHeader..."); }  //$NON-NLS-1$
		    consumeInterfaceMethodDeclaration(true); 			break;
 
    case 316 : if (DEBUG) { System.out.println("InvalidConstructorDeclaration ::= ConstructorHeader..."); }  //$NON-NLS-1$
		    consumeInvalidConstructorDeclaration(true); 			break;
 
    case 317 : if (DEBUG) { System.out.println("InvalidConstructorDeclaration ::= ConstructorHeader..."); }  //$NON-NLS-1$
		    consumeInvalidConstructorDeclaration(false); 			break;
 
    case 328 : if (DEBUG) { System.out.println("RecordDeclaration ::= RecordHeaderPart RecordBody"); }  //$NON-NLS-1$
		    consumeRecordDeclaration(); 			break;
 
    case 329 : if (DEBUG) { System.out.println("RecordHeaderPart ::= RecordHeaderName RecordHeader..."); }  //$NON-NLS-1$
		    consumeRecordHeaderPart(); 			break;
 
    case 330 : if (DEBUG) { System.out.println("RecordHeaderName ::= RecordHeaderName1 TypeParameters"); }  //$NON-NLS-1$
		    consumeRecordHeaderNameWithTypeParameters(); 			break;
 
    case 332 : if (DEBUG) { System.out.println("RecordHeaderName1 ::= Modifiersopt..."); }  //$NON-NLS-1$
		    consumeRecordHeaderName1(); 			break;
 
    case 333 : if (DEBUG) { System.out.println("RecordComponentHeaderRightParen ::= RPAREN"); }  //$NON-NLS-1$
		    consumeRecordComponentHeaderRightParen(); 			break;
 
    case 334 : if (DEBUG) { System.out.println("RecordHeader ::= LPAREN RecordComponentsopt..."); }  //$NON-NLS-1$
		    consumeRecordHeader(); 			break;
 
    case 335 : if (DEBUG) { System.out.println("RecordComponentsopt ::="); }  //$NON-NLS-1$
		    consumeRecordComponentsopt(); 			break;
 
    case 338 : if (DEBUG) { System.out.println("RecordComponents ::= RecordComponents COMMA..."); }  //$NON-NLS-1$
		    consumeRecordComponents(); 			break;
 
    case 340 : if (DEBUG) { System.out.println("RecordComponent ::= Modifiersopt Type..."); }  //$NON-NLS-1$
		    consumeRecordComponent(false); 			break;
 
    case 341 : if (DEBUG) { System.out.println("VariableArityRecordComponent ::= Modifiersopt Type..."); }  //$NON-NLS-1$
		    consumeRecordComponent(true); 			break;
 
    case 342 : if (DEBUG) { System.out.println("VariableArityRecordComponent ::= Modifiersopt Type..."); }  //$NON-NLS-1$
		    consumeRecordComponent(true); 			break;
 
    case 343 : if (DEBUG) { System.out.println("RecordBody ::= LBRACE RecordBodyDeclarationopt RBRACE"); }  //$NON-NLS-1$
		    consumeRecordBody(); 			break;
 
    case 344 : if (DEBUG) { System.out.println("RecordBodyDeclarationopt ::="); }  //$NON-NLS-1$
		    consumeEmptyRecordBodyDeclaration(); 			break;
 
    case 347 : if (DEBUG) { System.out.println("RecordBodyDeclarations ::= RecordBodyDeclarations..."); }  //$NON-NLS-1$
		    consumeRecordBodyDeclarations(); 			break;
 
    case 348 : if (DEBUG) { System.out.println("RecordBodyDeclaration ::= ClassBodyDeclaration"); }  //$NON-NLS-1$
		    consumeRecordBodyDeclaration(); 			break;
 
    case 349 : if (DEBUG) { System.out.println("RecordBodyDeclaration ::= CompactConstructorDeclaration"); }  //$NON-NLS-1$
		    consumeRecordBodyDeclaration(); 			break;
 
    case 350 : if (DEBUG) { System.out.println("CompactConstructorDeclaration ::=..."); }  //$NON-NLS-1$
		    consumeCompactConstructorDeclaration(); 			break;
 
    case 351 : if (DEBUG) { System.out.println("CompactConstructorHeader ::=..."); }  //$NON-NLS-1$
		    consumeCompactConstructorHeader(); 			break;
 
    case 352 : if (DEBUG) { System.out.println("CompactConstructorHeaderName ::= Modifiersopt Identifier"); }  //$NON-NLS-1$
		    consumeCompactConstructorHeaderName(); 			break;
 
    case 353 : if (DEBUG) { System.out.println("CompactConstructorHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
		    consumeCompactConstructorHeaderNameWithTypeParameters(); 			break;
 
    case 355 : if (DEBUG) { System.out.println("InstanceofExpression ::= InstanceofExpression..."); }  //$NON-NLS-1$
		    consumeInstanceOfExpression(); 			break;
 
    case 358 : if (DEBUG) { System.out.println("InstanceofClassic ::= instanceof Modifiersopt Type"); }  //$NON-NLS-1$
		    consumeInstanceOfClassic(); 			break;
 
    case 359 : if (DEBUG) { System.out.println("InstanceofPattern ::= instanceof Pattern"); }  //$NON-NLS-1$
		    consumeInstanceofPattern(); 			break;
 
    case 362 : if (DEBUG) { System.out.println("TypePattern ::= Modifiersopt Type Identifier"); }  //$NON-NLS-1$
		    consumeTypePattern(); 			break;
 
    case 363 : if (DEBUG) { System.out.println("TypePattern ::= Modifiersopt Type UNDERSCORE"); }  //$NON-NLS-1$
		    consumeTypePattern(); 			break;
 
    case 364 : if (DEBUG) { System.out.println("RecordPattern ::= Modifiersopt ReferenceType PushLPAREN"); }  //$NON-NLS-1$
		    consumeRecordPattern(); 			break;
 
    case 365 : if (DEBUG) { System.out.println("ComponentPatternListopt ::="); }  //$NON-NLS-1$
		    consumePatternListopt(); 			break;
 
    case 368 : if (DEBUG) { System.out.println("ComponentPatternList ::= ComponentPatternList COMMA..."); }  //$NON-NLS-1$
		    consumePatternList();  			break;
 
    case 371 : if (DEBUG) { System.out.println("UnnamedPattern ::= UNDERSCORE"); }  //$NON-NLS-1$
		    consumeUnnamedPattern(); 			break;
 
    case 373 : if (DEBUG) { System.out.println("PushLeftBrace ::="); }  //$NON-NLS-1$
		    consumePushLeftBrace(); 			break;
 
    case 374 : if (DEBUG) { System.out.println("ArrayInitializer ::= LBRACE PushLeftBrace ,opt RBRACE"); }  //$NON-NLS-1$
		    consumeEmptyArrayInitializer(); 			break;
 
    case 375 : if (DEBUG) { System.out.println("ArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
		    consumeArrayInitializer(); 			break;
 
    case 376 : if (DEBUG) { System.out.println("ArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
		    consumeArrayInitializer(); 			break;
 
    case 378 : if (DEBUG) { System.out.println("VariableInitializers ::= VariableInitializers COMMA..."); }  //$NON-NLS-1$
		    consumeVariableInitializers(); 			break;
 
    case 379 : if (DEBUG) { System.out.println("Block ::= OpenBlock LBRACE BlockStatementsopt RBRACE"); }  //$NON-NLS-1$
		    consumeBlock(); 			break;
 
    case 380 : if (DEBUG) { System.out.println("OpenBlock ::="); }  //$NON-NLS-1$
		    consumeOpenBlock() ; 			break;
 
    case 381 : if (DEBUG) { System.out.println("BlockStatements ::= BlockStatement"); }  //$NON-NLS-1$
		    consumeBlockStatement() ; 			break;
 
    case 382 : if (DEBUG) { System.out.println("BlockStatements ::= BlockStatements BlockStatement"); }  //$NON-NLS-1$
		    consumeBlockStatements() ; 			break;
 
    case 390 : if (DEBUG) { System.out.println("BlockStatement ::= InterfaceDeclaration"); }  //$NON-NLS-1$
		    consumeInvalidInterfaceDeclaration(); 			break;
 
    case 391 : if (DEBUG) { System.out.println("BlockStatement ::= AnnotationTypeDeclaration"); }  //$NON-NLS-1$
		    consumeInvalidAnnotationTypeDeclaration(); 			break;
 
    case 392 : if (DEBUG) { System.out.println("BlockStatement ::= EnumDeclaration"); }  //$NON-NLS-1$
		    consumeInvalidEnumDeclaration(); 			break;
 
    case 393 : if (DEBUG) { System.out.println("LocalVariableDeclarationStatement ::=..."); }  //$NON-NLS-1$
		    consumeLocalVariableDeclarationStatement(); 			break;
 
    case 394 : if (DEBUG) { System.out.println("LocalVariableDeclaration ::= Type PushModifiers..."); }  //$NON-NLS-1$
		    consumeLocalVariableDeclaration(); 			break;
 
    case 395 : if (DEBUG) { System.out.println("LocalVariableDeclaration ::= Modifiers Type..."); }  //$NON-NLS-1$
		    consumeLocalVariableDeclaration(); 			break;
 
    case 396 : if (DEBUG) { System.out.println("PushModifiers ::="); }  //$NON-NLS-1$
		    consumePushModifiers(); 			break;
 
    case 397 : if (DEBUG) { System.out.println("PushModifiersForHeader ::="); }  //$NON-NLS-1$
		    consumePushModifiersForHeader(); 			break;
 
    case 398 : if (DEBUG) { System.out.println("PushRealModifiers ::="); }  //$NON-NLS-1$
		    consumePushRealModifiers(); 			break;
 
    case 426 : if (DEBUG) { System.out.println("EmptyStatement ::= SEMICOLON"); }  //$NON-NLS-1$
		    consumeEmptyStatement(); 			break;
 
    case 427 : if (DEBUG) { System.out.println("LabeledStatement ::= Label COLON Statement"); }  //$NON-NLS-1$
		    consumeStatementLabel() ; 			break;
 
    case 428 : if (DEBUG) { System.out.println("LabeledStatementNoShortIf ::= Label COLON..."); }  //$NON-NLS-1$
		    consumeStatementLabel() ; 			break;
 
    case 429 : if (DEBUG) { System.out.println("Label ::= Identifier"); }  //$NON-NLS-1$
		    consumeLabel() ; 			break;
 
     case 430 : if (DEBUG) { System.out.println("ExpressionStatement ::= StatementExpression SEMICOLON"); }  //$NON-NLS-1$
		    consumeExpressionStatement(); 			break;
 
    case 439 : if (DEBUG) { System.out.println("PostExpressionInSwitchStatement ::="); }  //$NON-NLS-1$
		    consumePostExpressionInSwitch(true); 			break;
 
    case 440 : if (DEBUG) { System.out.println("PostExpressionInSwitchExpression ::="); }  //$NON-NLS-1$
		    consumePostExpressionInSwitch(false); 			break;
 
    case 441 : if (DEBUG) { System.out.println("PostExpressionInIf ::="); }  //$NON-NLS-1$
		    consumePostExpressionInIf(); 			break;
 
    case 442 : if (DEBUG) { System.out.println("PostExpressionInWhile ::="); }  //$NON-NLS-1$
		    consumePostExpressionInWhile(); 			break;
 
    case 443 : if (DEBUG) { System.out.println("IfThenStatement ::= if LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
		    consumeStatementIfNoElse(); 			break;
 
    case 444 : if (DEBUG) { System.out.println("IfThenElseStatement ::= if LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
		    consumeStatementIfWithElse(); 			break;
 
    case 445 : if (DEBUG) { System.out.println("IfThenElseStatementNoShortIf ::= if LPAREN Expression..."); }  //$NON-NLS-1$
		    consumeStatementIfWithElse(); 			break;
 
    case 446 : if (DEBUG) { System.out.println("SwitchStatement ::= switch LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
		    consumeSwitchStatementOrExpression(true) ; 			break;
 
    case 447 : if (DEBUG) { System.out.println("SwitchBlock ::= LBRACE RBRACE"); }  //$NON-NLS-1$
		    consumeSwitchBlock(false) ; 			break;
 
    case 450 : if (DEBUG) { System.out.println("SwitchBlock ::= LBRACE SwitchBlockStatements..."); }  //$NON-NLS-1$
		    consumeSwitchBlock(true) ; 			break;
 
    case 452 : if (DEBUG) { System.out.println("SwitchBlockStatements ::= SwitchBlockStatements..."); }  //$NON-NLS-1$
		    consumeSwitchBlockStatements() ; 			break;
 
    case 454 : if (DEBUG) { System.out.println("SwitchBlockStatement ::= SwitchLabels BlockStatements"); }  //$NON-NLS-1$
		    consumeSwitchBlockStatement() ; 			break;
 
    case 455 : if (DEBUG) { System.out.println("SwitchLabels ::= SwitchLabel COLON"); }  //$NON-NLS-1$
		    consumeSwitchLabels(false, false) ; 			break;
 
    case 456 : if (DEBUG) { System.out.println("SwitchLabels ::= SwitchLabels SwitchLabel COLON"); }  //$NON-NLS-1$
		    consumeSwitchLabels(true, false) ; 			break;
 
    case 457 : if (DEBUG) { System.out.println("PostCaseArrow ::="); }  //$NON-NLS-1$
		    consumeSwitchLabels(false, true) ; 			break;
 
     case 459 : if (DEBUG) { System.out.println("SwitchLabel ::= default"); }  //$NON-NLS-1$
		    consumeDefaultLabel(); 			break;
 
    case 462 : if (DEBUG) { System.out.println("SwitchExpression ::= switch LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
		    consumeSwitchStatementOrExpression(false) ; 			break;
 
     case 463 : if (DEBUG) { System.out.println("SwitchRule ::= SwitchLabel CaseArrow PostCaseArrow..."); }  //$NON-NLS-1$
		    consumeSwitchRule(SwitchRuleKind.EXPRESSION); 			break;
 
     case 464 : if (DEBUG) { System.out.println("SwitchRule ::= SwitchLabel CaseArrow PostCaseArrow..."); }  //$NON-NLS-1$
		    consumeSwitchRule(SwitchRuleKind.BLOCK); 			break;
 
     case 465 : if (DEBUG) { System.out.println("SwitchRule ::= SwitchLabel CaseArrow PostCaseArrow..."); }  //$NON-NLS-1$
		    consumeSwitchRule(SwitchRuleKind.THROW); 			break;
 
    case 467 : if (DEBUG) { System.out.println("CaseLabelElements ::= CaseLabelElements COMMA..."); }  //$NON-NLS-1$
		    consumeCaseLabelElements(); 			break;
 
    case 468 : if (DEBUG) { System.out.println("CaseLabelElement ::= ConstantExpression"); }  //$NON-NLS-1$
		    consumeCaseLabelElement(CaseLabelKind.CASE_EXPRESSION); 			break;
 
    case 469 : if (DEBUG) { System.out.println("CaseLabelElement ::= default"); }  //$NON-NLS-1$
		    consumeCaseLabelElement(CaseLabelKind.CASE_DEFAULT); 			break;
 
    case 470 : if (DEBUG) { System.out.println("CaseLabelElement ::= CaseLabelElementPattern"); }  //$NON-NLS-1$
		    consumeCaseLabelElement(CaseLabelKind.CASE_PATTERN); 			break;
 
    case 471 : if (DEBUG) { System.out.println("CaseLabelElement ::= CaseLabelElementPattern Guard"); }  //$NON-NLS-1$
		    consumeCaseLabelElement(CaseLabelKind.CASE_PATTERN); 			break;
 
    case 473 : if (DEBUG) { System.out.println("Guard ::= RestrictedIdentifierWhen Expression"); }  //$NON-NLS-1$
		    consumeGuard(); 			break;
 
    case 474 : if (DEBUG) { System.out.println("YieldStatement ::= RestrictedIdentifierYield Expression"); }  //$NON-NLS-1$
		    consumeStatementYield() ; 			break;
 
    case 475 : if (DEBUG) { System.out.println("WhileStatement ::= while LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
		    consumeStatementWhile() ; 			break;
 
    case 476 : if (DEBUG) { System.out.println("WhileStatementNoShortIf ::= while LPAREN Expression..."); }  //$NON-NLS-1$
		    consumeStatementWhile() ; 			break;
 
    case 477 : if (DEBUG) { System.out.println("DoStatement ::= do Statement while LPAREN Expression..."); }  //$NON-NLS-1$
		    consumeStatementDo() ; 			break;
 
    case 478 : if (DEBUG) { System.out.println("ForStatement ::= for LPAREN ForInitopt SEMICOLON..."); }  //$NON-NLS-1$
		    consumeStatementFor() ; 			break;
 
    case 479 : if (DEBUG) { System.out.println("ForStatementNoShortIf ::= for LPAREN ForInitopt..."); }  //$NON-NLS-1$
		    consumeStatementFor() ; 			break;
 
    case 480 : if (DEBUG) { System.out.println("ForInit ::= StatementExpressionList"); }  //$NON-NLS-1$
		    consumeForInit() ; 			break;
 
    case 484 : if (DEBUG) { System.out.println("StatementExpressionList ::= StatementExpressionList..."); }  //$NON-NLS-1$
		    consumeStatementExpressionList() ; 			break;
 
    case 485 : if (DEBUG) { System.out.println("AssertStatement ::= assert Expression SEMICOLON"); }  //$NON-NLS-1$
		    consumeSimpleAssertStatement() ; 			break;
 
    case 486 : if (DEBUG) { System.out.println("AssertStatement ::= assert Expression COLON Expression"); }  //$NON-NLS-1$
		    consumeAssertStatement() ; 			break;
 
    case 487 : if (DEBUG) { System.out.println("BreakStatement ::= break SEMICOLON"); }  //$NON-NLS-1$
		    consumeStatementBreak() ; 			break;
 
    case 488 : if (DEBUG) { System.out.println("BreakStatement ::= break Identifier SEMICOLON"); }  //$NON-NLS-1$
		    consumeStatementBreakWithLabel() ; 			break;
 
    case 489 : if (DEBUG) { System.out.println("ContinueStatement ::= continue SEMICOLON"); }  //$NON-NLS-1$
		    consumeStatementContinue() ; 			break;
 
    case 490 : if (DEBUG) { System.out.println("ContinueStatement ::= continue Identifier SEMICOLON"); }  //$NON-NLS-1$
		    consumeStatementContinueWithLabel() ; 			break;
 
    case 491 : if (DEBUG) { System.out.println("ReturnStatement ::= return Expressionopt SEMICOLON"); }  //$NON-NLS-1$
		    consumeStatementReturn() ; 			break;
 
    case 492 : if (DEBUG) { System.out.println("ThrowStatement ::= throw Expression SEMICOLON"); }  //$NON-NLS-1$
		    consumeStatementThrow(); 			break;
 
    case 493 : if (DEBUG) { System.out.println("SynchronizedStatement ::= OnlySynchronized LPAREN..."); }  //$NON-NLS-1$
		    consumeStatementSynchronized(); 			break;
 
    case 494 : if (DEBUG) { System.out.println("OnlySynchronized ::= synchronized"); }  //$NON-NLS-1$
		    consumeOnlySynchronized(); 			break;
 
    case 495 : if (DEBUG) { System.out.println("TryStatement ::= try TryBlock Catches"); }  //$NON-NLS-1$
		    consumeStatementTry(false, false); 			break;
 
    case 496 : if (DEBUG) { System.out.println("TryStatement ::= try TryBlock Catchesopt Finally"); }  //$NON-NLS-1$
		    consumeStatementTry(true, false); 			break;
 
    case 497 : if (DEBUG) { System.out.println("TryStatementWithResources ::= try ResourceSpecification"); }  //$NON-NLS-1$
		    consumeStatementTry(false, true); 			break;
 
    case 498 : if (DEBUG) { System.out.println("TryStatementWithResources ::= try ResourceSpecification"); }  //$NON-NLS-1$
		    consumeStatementTry(true, true); 			break;
 
    case 499 : if (DEBUG) { System.out.println("ResourceSpecification ::= LPAREN Resources ;opt RPAREN"); }  //$NON-NLS-1$
		    consumeResourceSpecification(); 			break;
 
    case 500 : if (DEBUG) { System.out.println(";opt ::="); }  //$NON-NLS-1$
		    consumeResourceOptionalTrailingSemiColon(false); 			break;
 
    case 501 : if (DEBUG) { System.out.println(";opt ::= SEMICOLON"); }  //$NON-NLS-1$
		    consumeResourceOptionalTrailingSemiColon(true); 			break;
 
    case 502 : if (DEBUG) { System.out.println("Resources ::= Resource"); }  //$NON-NLS-1$
		    consumeSingleResource(); 			break;
 
    case 503 : if (DEBUG) { System.out.println("Resources ::= Resources TrailingSemiColon Resource"); }  //$NON-NLS-1$
		    consumeMultipleResources(); 			break;
 
    case 504 : if (DEBUG) { System.out.println("TrailingSemiColon ::= SEMICOLON"); }  //$NON-NLS-1$
		    consumeResourceOptionalTrailingSemiColon(true); 			break;
 
    case 505 : if (DEBUG) { System.out.println("Resource ::= Type PushModifiers VariableDeclaratorId..."); }  //$NON-NLS-1$
		    consumeResourceAsLocalVariableDeclaration(); 			break;
 
    case 506 : if (DEBUG) { System.out.println("Resource ::= Modifiers Type PushRealModifiers..."); }  //$NON-NLS-1$
		    consumeResourceAsLocalVariableDeclaration(); 			break;
 
    case 507 : if (DEBUG) { System.out.println("Resource ::= Name"); }  //$NON-NLS-1$
		    consumeResourceAsLocalVariable(); 			break;
 
    case 508 : if (DEBUG) { System.out.println("Resource ::= this"); }  //$NON-NLS-1$
		    consumeResourceAsThis(); 			break;
 
    case 509 : if (DEBUG) { System.out.println("Resource ::= FieldAccess"); }  //$NON-NLS-1$
		    consumeResourceAsFieldAccess(); 			break;
 
    case 511 : if (DEBUG) { System.out.println("ExitTryBlock ::="); }  //$NON-NLS-1$
		    consumeExitTryBlock(); 			break;
 
    case 513 : if (DEBUG) { System.out.println("Catches ::= Catches CatchClause"); }  //$NON-NLS-1$
		    consumeCatches(); 			break;
 
    case 514 : if (DEBUG) { System.out.println("CatchClause ::= catch LPAREN CatchFormalParameter RPAREN"); }  //$NON-NLS-1$
		    consumeStatementCatch() ; 			break;
 
    case 516 : if (DEBUG) { System.out.println("PushLPAREN ::= LPAREN"); }  //$NON-NLS-1$
		    consumeLeftParen(); 			break;
 
    case 517 : if (DEBUG) { System.out.println("PushRPAREN ::= RPAREN"); }  //$NON-NLS-1$
		    consumeRightParen(); 			break;
 
    case 522 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= this"); }  //$NON-NLS-1$
		    consumePrimaryNoNewArrayThis(); 			break;
 
    case 523 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PushLPAREN Expression_NotName..."); }  //$NON-NLS-1$
		    consumePrimaryNoNewArray(); 			break;
 
    case 524 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PushLPAREN Name PushRPAREN"); }  //$NON-NLS-1$
		    consumePrimaryNoNewArrayWithName(); 			break;
 
    case 527 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name DOT this"); }  //$NON-NLS-1$
		    consumePrimaryNoNewArrayNameThis(); 			break;
 
    case 528 : if (DEBUG) { System.out.println("QualifiedSuperReceiver ::= Name DOT super"); }  //$NON-NLS-1$
		    consumeQualifiedSuperReceiver(); 			break;
 
    case 529 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name DOT class"); }  //$NON-NLS-1$
		    consumePrimaryNoNewArrayName(); 			break;
 
    case 530 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name Dims DOT class"); }  //$NON-NLS-1$
		    consumePrimaryNoNewArrayArrayType(); 			break;
 
    case 531 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PrimitiveType Dims DOT class"); }  //$NON-NLS-1$
		    consumePrimaryNoNewArrayPrimitiveArrayType(); 			break;
 
    case 532 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PrimitiveType DOT class"); }  //$NON-NLS-1$
		    consumePrimaryNoNewArrayPrimitiveType(); 			break;
 
    case 538 : if (DEBUG) { System.out.println("ReferenceExpressionTypeArgumentsAndTrunk0 ::=..."); }  //$NON-NLS-1$
		    consumeReferenceExpressionTypeArgumentsAndTrunk(false); 			break;
 
    case 539 : if (DEBUG) { System.out.println("ReferenceExpressionTypeArgumentsAndTrunk0 ::=..."); }  //$NON-NLS-1$
		    consumeReferenceExpressionTypeArgumentsAndTrunk(true); 			break;
 
    case 540 : if (DEBUG) { System.out.println("ReferenceExpression ::= PrimitiveType Dims COLON_COLON"); }  //$NON-NLS-1$
		    consumeReferenceExpressionTypeForm(true); 			break;
 
    case 541 : if (DEBUG) { System.out.println("ReferenceExpression ::= Name Dimsopt COLON_COLON..."); }  //$NON-NLS-1$
		    consumeReferenceExpressionTypeForm(false); 			break;
 
    case 542 : if (DEBUG) { System.out.println("ReferenceExpression ::= Name BeginTypeArguments..."); }  //$NON-NLS-1$
		    consumeReferenceExpressionGenericTypeForm(); 			break;
 
    case 543 : if (DEBUG) { System.out.println("ReferenceExpression ::= Primary COLON_COLON..."); }  //$NON-NLS-1$
		    consumeReferenceExpressionPrimaryForm(); 			break;
 
    case 544 : if (DEBUG) { System.out.println("ReferenceExpression ::= QualifiedSuperReceiver..."); }  //$NON-NLS-1$
		    consumeReferenceExpressionPrimaryForm(); 			break;
 
    case 545 : if (DEBUG) { System.out.println("ReferenceExpression ::= super COLON_COLON..."); }  //$NON-NLS-1$
		    consumeReferenceExpressionSuperForm(); 			break;
 
    case 546 : if (DEBUG) { System.out.println("NonWildTypeArgumentsopt ::="); }  //$NON-NLS-1$
		    consumeEmptyTypeArguments(); 			break;
 
    case 548 : if (DEBUG) { System.out.println("IdentifierOrNew ::= Identifier"); }  //$NON-NLS-1$
		    consumeIdentifierOrNew(false); 			break;
 
    case 549 : if (DEBUG) { System.out.println("IdentifierOrNew ::= new"); }  //$NON-NLS-1$
		    consumeIdentifierOrNew(true); 			break;
 
    case 550 : if (DEBUG) { System.out.println("LambdaExpression ::= LambdaParameters ARROW LambdaBody"); }  //$NON-NLS-1$
		    consumeLambdaExpression(); 			break;
 
    case 551 : if (DEBUG) { System.out.println("NestedLambda ::="); }  //$NON-NLS-1$
		    consumeNestedLambda(); 			break;
 
    case 552 : if (DEBUG) { System.out.println("LambdaParameters ::= UNDERSCORE NestedLambda"); }  //$NON-NLS-1$
		    consumeTypeElidedLambdaParameter(false); 			break;
 
    case 553 : if (DEBUG) { System.out.println("LambdaParameters ::= Identifier NestedLambda"); }  //$NON-NLS-1$
		    consumeTypeElidedLambdaParameter(false); 			break;
 
    case 559 : if (DEBUG) { System.out.println("TypeElidedFormalParameterList ::=..."); }  //$NON-NLS-1$
		    consumeFormalParameterList(); 			break;
 
    case 560 : if (DEBUG) { System.out.println("TypeElidedFormalParameter ::= Modifiersopt Identifier"); }  //$NON-NLS-1$
		    consumeTypeElidedLambdaParameter(true); 			break;
 
    case 561 : if (DEBUG) { System.out.println("TypeElidedFormalParameter ::= UNDERSCORE"); }  //$NON-NLS-1$
		    consumeBracketedTypeElidedUnderscoreLambdaParameter(); 			break;
 
    case 564 : if (DEBUG) { System.out.println("ElidedLeftBraceAndReturn ::="); }  //$NON-NLS-1$
		    consumeElidedLeftBraceAndReturn(); 			break;
 
    case 565 : if (DEBUG) { System.out.println("AllocationHeader ::= new ClassType LPAREN..."); }  //$NON-NLS-1$
		    consumeAllocationHeader(); 			break;
 
    case 566 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= new..."); }  //$NON-NLS-1$
		    consumeClassInstanceCreationExpressionWithTypeArguments(); 			break;
 
    case 567 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= new ClassType..."); }  //$NON-NLS-1$
		    consumeClassInstanceCreationExpression(); 			break;
 
    case 568 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= Primary DOT new..."); }  //$NON-NLS-1$
		    consumeClassInstanceCreationExpressionQualifiedWithTypeArguments() ; 			break;
 
    case 569 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= Primary DOT new..."); }  //$NON-NLS-1$
		    consumeClassInstanceCreationExpressionQualified() ; 			break;
 
    case 570 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::=..."); }  //$NON-NLS-1$
		    consumeClassInstanceCreationExpressionQualified() ; 			break;
 
    case 571 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::=..."); }  //$NON-NLS-1$
		    consumeClassInstanceCreationExpressionQualifiedWithTypeArguments() ; 			break;
 
    case 572 : if (DEBUG) { System.out.println("EnterInstanceCreationArgumentList ::="); }  //$NON-NLS-1$
		    consumeEnterInstanceCreationArgumentList(); 			break;
 
    case 573 : if (DEBUG) { System.out.println("ClassInstanceCreationExpressionName ::= Name DOT new"); }  //$NON-NLS-1$
		    consumeClassInstanceCreationExpressionName() ; 			break;
 
    case 574 : if (DEBUG) { System.out.println("UnqualifiedClassBodyopt ::="); }  //$NON-NLS-1$
		    consumeClassBodyopt(); 			break;
 
    case 576 : if (DEBUG) { System.out.println("UnqualifiedEnterAnonymousClassBody ::="); }  //$NON-NLS-1$
		    consumeEnterAnonymousClassBody(false); 			break;
 
    case 577 : if (DEBUG) { System.out.println("QualifiedClassBodyopt ::="); }  //$NON-NLS-1$
		    consumeClassBodyopt(); 			break;
 
    case 579 : if (DEBUG) { System.out.println("QualifiedEnterAnonymousClassBody ::="); }  //$NON-NLS-1$
		    consumeEnterAnonymousClassBody(true); 			break;
 
    case 581 : if (DEBUG) { System.out.println("ArgumentList ::= ArgumentList COMMA Expression"); }  //$NON-NLS-1$
		    consumeArgumentList(); 			break;
 
    case 582 : if (DEBUG) { System.out.println("ArrayCreationHeader ::= new PrimitiveType..."); }  //$NON-NLS-1$
		    consumeArrayCreationHeader(); 			break;
 
    case 583 : if (DEBUG) { System.out.println("ArrayCreationHeader ::= new ClassOrInterfaceType..."); }  //$NON-NLS-1$
		    consumeArrayCreationHeader(); 			break;
 
    case 584 : if (DEBUG) { System.out.println("ArrayCreationWithoutArrayInitializer ::= new..."); }  //$NON-NLS-1$
		    consumeArrayCreationExpressionWithoutInitializer(); 			break;
 
    case 585 : if (DEBUG) { System.out.println("ArrayCreationWithArrayInitializer ::= new PrimitiveType"); }  //$NON-NLS-1$
		    consumeArrayCreationExpressionWithInitializer(); 			break;
 
    case 586 : if (DEBUG) { System.out.println("ArrayCreationWithoutArrayInitializer ::= new..."); }  //$NON-NLS-1$
		    consumeArrayCreationExpressionWithoutInitializer(); 			break;
 
    case 587 : if (DEBUG) { System.out.println("ArrayCreationWithArrayInitializer ::= new..."); }  //$NON-NLS-1$
		    consumeArrayCreationExpressionWithInitializer(); 			break;
 
    case 589 : if (DEBUG) { System.out.println("DimWithOrWithOutExprs ::= DimWithOrWithOutExprs..."); }  //$NON-NLS-1$
		    consumeDimWithOrWithOutExprs(); 			break;
 
     case 591 : if (DEBUG) { System.out.println("DimWithOrWithOutExpr ::= TypeAnnotationsopt LBRACKET..."); }  //$NON-NLS-1$
		    consumeDimWithOrWithOutExpr(); 			break;
 
     case 592 : if (DEBUG) { System.out.println("Dims ::= DimsLoop"); }  //$NON-NLS-1$
		    consumeDims(); 			break;
 
     case 595 : if (DEBUG) { System.out.println("OneDimLoop ::= LBRACKET RBRACKET"); }  //$NON-NLS-1$
		    consumeOneDimLoop(false); 			break;
 
     case 596 : if (DEBUG) { System.out.println("OneDimLoop ::= TypeAnnotations LBRACKET RBRACKET"); }  //$NON-NLS-1$
		    consumeOneDimLoop(true); 			break;
 
    case 597 : if (DEBUG) { System.out.println("FieldAccess ::= Primary DOT Identifier"); }  //$NON-NLS-1$
		    consumeFieldAccess(false); 			break;
 
    case 598 : if (DEBUG) { System.out.println("FieldAccess ::= super DOT Identifier"); }  //$NON-NLS-1$
		    consumeFieldAccess(true); 			break;
 
    case 599 : if (DEBUG) { System.out.println("FieldAccess ::= QualifiedSuperReceiver DOT Identifier"); }  //$NON-NLS-1$
		    consumeFieldAccess(false); 			break;
 
    case 600 : if (DEBUG) { System.out.println("MethodInvocation ::= Name LPAREN ArgumentListopt RPAREN"); }  //$NON-NLS-1$
		    consumeMethodInvocationName(); 			break;
 
    case 601 : if (DEBUG) { System.out.println("MethodInvocation ::= Name DOT OnlyTypeArguments..."); }  //$NON-NLS-1$
		    consumeMethodInvocationNameWithTypeArguments(); 			break;
 
    case 602 : if (DEBUG) { System.out.println("MethodInvocation ::= Primary DOT OnlyTypeArguments..."); }  //$NON-NLS-1$
		    consumeMethodInvocationPrimaryWithTypeArguments(); 			break;
 
    case 603 : if (DEBUG) { System.out.println("MethodInvocation ::= Primary DOT Identifier LPAREN..."); }  //$NON-NLS-1$
		    consumeMethodInvocationPrimary(); 			break;
 
    case 604 : if (DEBUG) { System.out.println("MethodInvocation ::= QualifiedSuperReceiver DOT..."); }  //$NON-NLS-1$
		    consumeMethodInvocationPrimary(); 			break;
 
    case 605 : if (DEBUG) { System.out.println("MethodInvocation ::= QualifiedSuperReceiver DOT..."); }  //$NON-NLS-1$
		    consumeMethodInvocationPrimaryWithTypeArguments(); 			break;
 
    case 606 : if (DEBUG) { System.out.println("MethodInvocation ::= super DOT OnlyTypeArguments..."); }  //$NON-NLS-1$
		    consumeMethodInvocationSuperWithTypeArguments(); 			break;
 
    case 607 : if (DEBUG) { System.out.println("MethodInvocation ::= super DOT Identifier LPAREN..."); }  //$NON-NLS-1$
		    consumeMethodInvocationSuper(); 			break;
 
    case 608 : if (DEBUG) { System.out.println("ArrayAccess ::= Name LBRACKET Expression RBRACKET"); }  //$NON-NLS-1$
		    consumeArrayAccess(true); 			break;
 
    case 609 : if (DEBUG) { System.out.println("ArrayAccess ::= PrimaryNoNewArray LBRACKET Expression..."); }  //$NON-NLS-1$
		    consumeArrayAccess(false); 			break;
 
    case 610 : if (DEBUG) { System.out.println("ArrayAccess ::= ArrayCreationWithArrayInitializer..."); }  //$NON-NLS-1$
		    consumeArrayAccess(false); 			break;
 
    case 612 : if (DEBUG) { System.out.println("PostfixExpression ::= Name"); }  //$NON-NLS-1$
		    consumePostfixExpression(); 			break;
 
    case 615 : if (DEBUG) { System.out.println("PostIncrementExpression ::= PostfixExpression PLUS_PLUS"); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.PLUS,true); 			break;
 
    case 616 : if (DEBUG) { System.out.println("PostDecrementExpression ::= PostfixExpression..."); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.MINUS,true); 			break;
 
    case 617 : if (DEBUG) { System.out.println("PushPosition ::="); }  //$NON-NLS-1$
		    consumePushPosition(); 			break;
 
    case 620 : if (DEBUG) { System.out.println("UnaryExpression ::= PLUS PushPosition UnaryExpression"); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.PLUS); 			break;
 
    case 621 : if (DEBUG) { System.out.println("UnaryExpression ::= MINUS PushPosition UnaryExpression"); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.MINUS); 			break;
 
    case 623 : if (DEBUG) { System.out.println("PreIncrementExpression ::= PLUS_PLUS PushPosition..."); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.PLUS,false); 			break;
 
    case 624 : if (DEBUG) { System.out.println("PreDecrementExpression ::= MINUS_MINUS PushPosition..."); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.MINUS,false); 			break;
 
    case 626 : if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus ::= TWIDDLE PushPosition..."); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.TWIDDLE); 			break;
 
    case 627 : if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus ::= NOT PushPosition..."); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.NOT); 			break;
 
    case 629 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN PrimitiveType Dimsopt..."); }  //$NON-NLS-1$
		    consumeCastExpressionWithPrimitiveType(); 			break;
 
    case 630 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name..."); }  //$NON-NLS-1$
		    consumeCastExpressionWithGenericsArray(); 			break;
 
    case 631 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name..."); }  //$NON-NLS-1$
		    consumeCastExpressionWithQualifiedGenericsArray(); 			break;
 
    case 632 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name PushRPAREN..."); }  //$NON-NLS-1$
		    consumeCastExpressionLL1(); 			break;
 
    case 633 : if (DEBUG) { System.out.println("CastExpression ::= BeginIntersectionCast PushLPAREN..."); }  //$NON-NLS-1$
		    consumeCastExpressionLL1WithBounds(); 			break;
 
    case 634 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name Dims..."); }  //$NON-NLS-1$
		    consumeCastExpressionWithNameArray(); 			break;
 
    case 635 : if (DEBUG) { System.out.println("AdditionalBoundsListOpt ::="); }  //$NON-NLS-1$
		    consumeZeroAdditionalBounds(); 			break;
 
    case 639 : if (DEBUG) { System.out.println("OnlyTypeArgumentsForCastExpression ::= OnlyTypeArguments"); }  //$NON-NLS-1$
		    consumeOnlyTypeArgumentsForCastExpression(); 			break;
 
    case 640 : if (DEBUG) { System.out.println("InsideCastExpression ::="); }  //$NON-NLS-1$
		    consumeInsideCastExpression(); 			break;
 
    case 641 : if (DEBUG) { System.out.println("InsideCastExpressionLL1 ::="); }  //$NON-NLS-1$
		    consumeInsideCastExpressionLL1(); 			break;
 
    case 642 : if (DEBUG) { System.out.println("InsideCastExpressionLL1WithBounds ::="); }  //$NON-NLS-1$
		    consumeInsideCastExpressionLL1WithBounds (); 			break;
 
    case 643 : if (DEBUG) { System.out.println("InsideCastExpressionWithQualifiedGenerics ::="); }  //$NON-NLS-1$
		    consumeInsideCastExpressionWithQualifiedGenerics(); 			break;
 
    case 645 : if (DEBUG) { System.out.println("MultiplicativeExpression ::= MultiplicativeExpression..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.MULTIPLY); 			break;
 
    case 646 : if (DEBUG) { System.out.println("MultiplicativeExpression ::= MultiplicativeExpression..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.DIVIDE); 			break;
 
    case 647 : if (DEBUG) { System.out.println("MultiplicativeExpression ::= MultiplicativeExpression..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.REMAINDER); 			break;
 
    case 649 : if (DEBUG) { System.out.println("AdditiveExpression ::= AdditiveExpression PLUS..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.PLUS); 			break;
 
    case 650 : if (DEBUG) { System.out.println("AdditiveExpression ::= AdditiveExpression MINUS..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.MINUS); 			break;
 
    case 652 : if (DEBUG) { System.out.println("ShiftExpression ::= ShiftExpression LEFT_SHIFT..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.LEFT_SHIFT); 			break;
 
    case 653 : if (DEBUG) { System.out.println("ShiftExpression ::= ShiftExpression RIGHT_SHIFT..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.RIGHT_SHIFT); 			break;
 
    case 654 : if (DEBUG) { System.out.println("ShiftExpression ::= ShiftExpression UNSIGNED_RIGHT_SHIFT"); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.UNSIGNED_RIGHT_SHIFT); 			break;
 
    case 656 : if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression LESS..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.LESS); 			break;
 
    case 657 : if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression GREATER..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.GREATER); 			break;
 
    case 658 : if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression LESS_EQUAL"); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.LESS_EQUAL); 			break;
 
    case 659 : if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.GREATER_EQUAL); 			break;
 
    case 661 : if (DEBUG) { System.out.println("EqualityExpression ::= EqualityExpression EQUAL_EQUAL..."); }  //$NON-NLS-1$
		    consumeEqualityExpression(OperatorIds.EQUAL_EQUAL); 			break;
 
    case 662 : if (DEBUG) { System.out.println("EqualityExpression ::= EqualityExpression NOT_EQUAL..."); }  //$NON-NLS-1$
		    consumeEqualityExpression(OperatorIds.NOT_EQUAL); 			break;
 
    case 664 : if (DEBUG) { System.out.println("AndExpression ::= AndExpression AND EqualityExpression"); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.AND); 			break;
 
    case 666 : if (DEBUG) { System.out.println("ExclusiveOrExpression ::= ExclusiveOrExpression XOR..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.XOR); 			break;
 
    case 668 : if (DEBUG) { System.out.println("InclusiveOrExpression ::= InclusiveOrExpression OR..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.OR); 			break;
 
    case 670 : if (DEBUG) { System.out.println("ConditionalAndExpression ::= ConditionalAndExpression..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.AND_AND); 			break;
 
    case 672 : if (DEBUG) { System.out.println("ConditionalOrExpression ::= ConditionalOrExpression..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.OR_OR); 			break;
 
    case 674 : if (DEBUG) { System.out.println("ConditionalExpression ::= ConditionalOrExpression..."); }  //$NON-NLS-1$
		    consumeConditionalExpression(OperatorIds.QUESTIONCOLON) ; 			break;
 
    case 677 : if (DEBUG) { System.out.println("Assignment ::= PostfixExpression AssignmentOperator..."); }  //$NON-NLS-1$
		    consumeAssignment(); 			break;
 
    case 679 : if (DEBUG) { System.out.println("Assignment ::= InvalidArrayInitializerAssignement"); }  //$NON-NLS-1$
		    ignoreExpressionAssignment();			break;
 
    case 680 : if (DEBUG) { System.out.println("AssignmentOperator ::= EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(EQUAL); 			break;
 
    case 681 : if (DEBUG) { System.out.println("AssignmentOperator ::= MULTIPLY_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(MULTIPLY); 			break;
 
    case 682 : if (DEBUG) { System.out.println("AssignmentOperator ::= DIVIDE_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(DIVIDE); 			break;
 
    case 683 : if (DEBUG) { System.out.println("AssignmentOperator ::= REMAINDER_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(REMAINDER); 			break;
 
    case 684 : if (DEBUG) { System.out.println("AssignmentOperator ::= PLUS_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(PLUS); 			break;
 
    case 685 : if (DEBUG) { System.out.println("AssignmentOperator ::= MINUS_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(MINUS); 			break;
 
    case 686 : if (DEBUG) { System.out.println("AssignmentOperator ::= LEFT_SHIFT_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(LEFT_SHIFT); 			break;
 
    case 687 : if (DEBUG) { System.out.println("AssignmentOperator ::= RIGHT_SHIFT_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(RIGHT_SHIFT); 			break;
 
    case 688 : if (DEBUG) { System.out.println("AssignmentOperator ::= UNSIGNED_RIGHT_SHIFT_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(UNSIGNED_RIGHT_SHIFT); 			break;
 
    case 689 : if (DEBUG) { System.out.println("AssignmentOperator ::= AND_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(AND); 			break;
 
    case 690 : if (DEBUG) { System.out.println("AssignmentOperator ::= XOR_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(XOR); 			break;
 
    case 691 : if (DEBUG) { System.out.println("AssignmentOperator ::= OR_EQUAL"); }  //$NON-NLS-1$
		    consumeAssignmentOperator(OR); 			break;
 
    case 692 : if (DEBUG) { System.out.println("Expression ::= AssignmentExpression"); }  //$NON-NLS-1$
		    consumeExpression(); 			break;
 
    case 695 : if (DEBUG) { System.out.println("Expressionopt ::="); }  //$NON-NLS-1$
		    consumeEmptyExpression(); 			break;
 
    case 700 : if (DEBUG) { System.out.println("ClassBodyDeclarationsopt ::="); }  //$NON-NLS-1$
		    consumeEmptyClassBodyDeclarationsopt(); 			break;
 
    case 701 : if (DEBUG) { System.out.println("ClassBodyDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
		    consumeClassBodyDeclarationsopt(); 			break;
 
     case 702 : if (DEBUG) { System.out.println("Modifiersopt ::="); }  //$NON-NLS-1$
		    consumeDefaultModifiers(); 			break;
 
    case 703 : if (DEBUG) { System.out.println("Modifiersopt ::= Modifiers"); }  //$NON-NLS-1$
		    consumeModifiers(); 			break;
 
    case 704 : if (DEBUG) { System.out.println("BlockStatementsopt ::="); }  //$NON-NLS-1$
		    consumeEmptyBlockStatementsopt(); 			break;
 
     case 706 : if (DEBUG) { System.out.println("Dimsopt ::="); }  //$NON-NLS-1$
		    consumeEmptyDimsopt(); 			break;
 
     case 708 : if (DEBUG) { System.out.println("ArgumentListopt ::="); }  //$NON-NLS-1$
		    consumeEmptyArgumentListopt(); 			break;
 
    case 712 : if (DEBUG) { System.out.println("FormalParameterListopt ::="); }  //$NON-NLS-1$
		    consumeFormalParameterListopt(); 			break;
 
    case 718 : if (DEBUG) { System.out.println("PermittedTypesopt ::= RestrictedIdentifierpermits..."); }  //$NON-NLS-1$
		    consumePermittedTypes(); 			break;
 
     case 719 : if (DEBUG) { System.out.println("InterfaceMemberDeclarationsopt ::="); }  //$NON-NLS-1$
		    consumeEmptyInterfaceMemberDeclarationsopt(); 			break;
 
     case 720 : if (DEBUG) { System.out.println("InterfaceMemberDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
		    consumeInterfaceMemberDeclarationsopt(); 			break;
 
    case 721 : if (DEBUG) { System.out.println("NestedType ::="); }  //$NON-NLS-1$
		    consumeNestedType(); 			break;

     case 722 : if (DEBUG) { System.out.println("ForInitopt ::="); }  //$NON-NLS-1$
		    consumeEmptyForInitopt(); 			break;
 
     case 724 : if (DEBUG) { System.out.println("ForUpdateopt ::="); }  //$NON-NLS-1$
		    consumeEmptyForUpdateopt(); 			break;
 
     case 728 : if (DEBUG) { System.out.println("Catchesopt ::="); }  //$NON-NLS-1$
		    consumeEmptyCatchesopt(); 			break;
 
     case 730 : if (DEBUG) { System.out.println("EnumDeclaration ::= EnumHeader EnumBody"); }  //$NON-NLS-1$
		    consumeEnumDeclaration(); 			break;
 
     case 731 : if (DEBUG) { System.out.println("EnumHeader ::= EnumHeaderName ClassHeaderImplementsopt"); }  //$NON-NLS-1$
		    consumeEnumHeader(); 			break;
 
     case 732 : if (DEBUG) { System.out.println("EnumHeaderName ::= Modifiersopt enum Identifier"); }  //$NON-NLS-1$
		    consumeEnumHeaderName(); 			break;
 
     case 733 : if (DEBUG) { System.out.println("EnumHeaderName ::= Modifiersopt enum Identifier..."); }  //$NON-NLS-1$
		    consumeEnumHeaderNameWithTypeParameters(); 			break;
 
     case 734 : if (DEBUG) { System.out.println("EnumBody ::= LBRACE EnumBodyDeclarationsopt RBRACE"); }  //$NON-NLS-1$
		    consumeEnumBodyNoConstants(); 			break;
 
     case 735 : if (DEBUG) { System.out.println("EnumBody ::= LBRACE COMMA EnumBodyDeclarationsopt..."); }  //$NON-NLS-1$
		    consumeEnumBodyNoConstants(); 			break;
 
     case 736 : if (DEBUG) { System.out.println("EnumBody ::= LBRACE EnumConstants COMMA..."); }  //$NON-NLS-1$
		    consumeEnumBodyWithConstants(); 			break;
 
     case 737 : if (DEBUG) { System.out.println("EnumBody ::= LBRACE EnumConstants..."); }  //$NON-NLS-1$
		    consumeEnumBodyWithConstants(); 			break;
 
    case 739 : if (DEBUG) { System.out.println("EnumConstants ::= EnumConstants COMMA EnumConstant"); }  //$NON-NLS-1$
		    consumeEnumConstants(); 			break;
 
    case 740 : if (DEBUG) { System.out.println("EnumConstantHeaderName ::= Modifiersopt Identifier"); }  //$NON-NLS-1$
		    consumeEnumConstantHeaderName(); 			break;
 
    case 741 : if (DEBUG) { System.out.println("EnumConstantHeader ::= EnumConstantHeaderName..."); }  //$NON-NLS-1$
		    consumeEnumConstantHeader(); 			break;
 
    case 742 : if (DEBUG) { System.out.println("EnumConstant ::= EnumConstantHeader ForceNoDiet..."); }  //$NON-NLS-1$
		    consumeEnumConstantWithClassBody(); 			break;
 
    case 743 : if (DEBUG) { System.out.println("EnumConstant ::= EnumConstantHeader"); }  //$NON-NLS-1$
		    consumeEnumConstantNoClassBody(); 			break;
 
    case 744 : if (DEBUG) { System.out.println("Arguments ::= LPAREN ArgumentListopt RPAREN"); }  //$NON-NLS-1$
		    consumeArguments(); 			break;
 
    case 745 : if (DEBUG) { System.out.println("Argumentsopt ::="); }  //$NON-NLS-1$
		    consumeEmptyArguments(); 			break;
 
    case 747 : if (DEBUG) { System.out.println("EnumDeclarations ::= SEMICOLON ClassBodyDeclarationsopt"); }  //$NON-NLS-1$
		    consumeEnumDeclarations(); 			break;
 
    case 748 : if (DEBUG) { System.out.println("EnumBodyDeclarationsopt ::="); }  //$NON-NLS-1$
		    consumeEmptyEnumDeclarations(); 			break;
 
    case 750 : if (DEBUG) { System.out.println("EnhancedForStatement ::= EnhancedForStatementHeader..."); }  //$NON-NLS-1$
		    consumeEnhancedForStatement(); 			break;
 
    case 751 : if (DEBUG) { System.out.println("EnhancedForStatementNoShortIf ::=..."); }  //$NON-NLS-1$
		    consumeEnhancedForStatement(); 			break;
 
    case 752 : if (DEBUG) { System.out.println("EnhancedForStatementHeaderInit ::= for LPAREN Type..."); }  //$NON-NLS-1$
		    consumeEnhancedForStatementHeaderInit(false); 			break;
 
    case 753 : if (DEBUG) { System.out.println("EnhancedForStatementHeaderInit ::= for LPAREN Modifiers"); }  //$NON-NLS-1$
		    consumeEnhancedForStatementHeaderInit(true); 			break;
 
    case 754 : if (DEBUG) { System.out.println("EnhancedForStatementHeader ::=..."); }  //$NON-NLS-1$
		    consumeEnhancedForStatementHeader(); 			break;
 
    case 755 : if (DEBUG) { System.out.println("SingleStaticImportDeclaration ::=..."); }  //$NON-NLS-1$
		    consumeImportDeclaration(); 			break;
 
    case 756 : if (DEBUG) { System.out.println("SingleStaticImportDeclarationName ::= import static Name"); }  //$NON-NLS-1$
		    consumeSingleStaticImportDeclarationName(); 			break;
 
    case 757 : if (DEBUG) { System.out.println("StaticImportOnDemandDeclaration ::=..."); }  //$NON-NLS-1$
		    consumeImportDeclaration(); 			break;
 
    case 758 : if (DEBUG) { System.out.println("StaticImportOnDemandDeclarationName ::= import static..."); }  //$NON-NLS-1$
		    consumeStaticImportOnDemandDeclarationName(); 			break;
 
    case 760 : if (DEBUG) { System.out.println("SingleModuleImportDeclaration ::=..."); }  //$NON-NLS-1$
		    consumeImportDeclaration(); 			break;
 
    case 761 : if (DEBUG) { System.out.println("SingleModuleImportDeclarationName ::= import module Name"); }  //$NON-NLS-1$
		    consumeSingleModuleImportDeclarationName(); 			break;
 
    case 762 : if (DEBUG) { System.out.println("TypeArguments ::= LESS TypeArgumentList1"); }  //$NON-NLS-1$
		    consumeTypeArguments(); 			break;
 
    case 763 : if (DEBUG) { System.out.println("OnlyTypeArguments ::= LESS TypeArgumentList1"); }  //$NON-NLS-1$
		    consumeOnlyTypeArguments(); 			break;
 
    case 765 : if (DEBUG) { System.out.println("TypeArgumentList1 ::= TypeArgumentList COMMA..."); }  //$NON-NLS-1$
		    consumeTypeArgumentList1(); 			break;
 
    case 767 : if (DEBUG) { System.out.println("TypeArgumentList ::= TypeArgumentList COMMA TypeArgument"); }  //$NON-NLS-1$
		    consumeTypeArgumentList(); 			break;
 
    case 768 : if (DEBUG) { System.out.println("TypeArgument ::= ReferenceType"); }  //$NON-NLS-1$
		    consumeTypeArgument(); 			break;
 
    case 772 : if (DEBUG) { System.out.println("ReferenceType1 ::= ReferenceType GREATER"); }  //$NON-NLS-1$
		    consumeReferenceType1(); 			break;
 
    case 773 : if (DEBUG) { System.out.println("ReferenceType1 ::= ClassOrInterface LESS..."); }  //$NON-NLS-1$
		    consumeTypeArgumentReferenceType1(); 			break;
 
    case 775 : if (DEBUG) { System.out.println("TypeArgumentList2 ::= TypeArgumentList COMMA..."); }  //$NON-NLS-1$
		    consumeTypeArgumentList2(); 			break;
 
    case 778 : if (DEBUG) { System.out.println("ReferenceType2 ::= ReferenceType RIGHT_SHIFT"); }  //$NON-NLS-1$
		    consumeReferenceType2(); 			break;
 
    case 779 : if (DEBUG) { System.out.println("ReferenceType2 ::= ClassOrInterface LESS..."); }  //$NON-NLS-1$
		    consumeTypeArgumentReferenceType2(); 			break;
 
    case 781 : if (DEBUG) { System.out.println("TypeArgumentList3 ::= TypeArgumentList COMMA..."); }  //$NON-NLS-1$
		    consumeTypeArgumentList3(); 			break;
 
    case 784 : if (DEBUG) { System.out.println("ReferenceType3 ::= ReferenceType UNSIGNED_RIGHT_SHIFT"); }  //$NON-NLS-1$
		    consumeReferenceType3(); 			break;
 
    case 785 : if (DEBUG) { System.out.println("Wildcard ::= TypeAnnotationsopt QUESTION"); }  //$NON-NLS-1$
		    consumeWildcard(); 			break;
 
    case 786 : if (DEBUG) { System.out.println("Wildcard ::= TypeAnnotationsopt QUESTION WildcardBounds"); }  //$NON-NLS-1$
		    consumeWildcardWithBounds(); 			break;
 
    case 787 : if (DEBUG) { System.out.println("WildcardBounds ::= extends ReferenceType"); }  //$NON-NLS-1$
		    consumeWildcardBoundsExtends(); 			break;
 
    case 788 : if (DEBUG) { System.out.println("WildcardBounds ::= super ReferenceType"); }  //$NON-NLS-1$
		    consumeWildcardBoundsSuper(); 			break;
 
    case 789 : if (DEBUG) { System.out.println("Wildcard1 ::= TypeAnnotationsopt QUESTION GREATER"); }  //$NON-NLS-1$
		    consumeWildcard1(); 			break;
 
    case 790 : if (DEBUG) { System.out.println("Wildcard1 ::= TypeAnnotationsopt QUESTION..."); }  //$NON-NLS-1$
		    consumeWildcard1WithBounds(); 			break;
 
    case 791 : if (DEBUG) { System.out.println("WildcardBounds1 ::= extends ReferenceType1"); }  //$NON-NLS-1$
		    consumeWildcardBounds1Extends(); 			break;
 
    case 792 : if (DEBUG) { System.out.println("WildcardBounds1 ::= super ReferenceType1"); }  //$NON-NLS-1$
		    consumeWildcardBounds1Super(); 			break;
 
    case 793 : if (DEBUG) { System.out.println("Wildcard2 ::= TypeAnnotationsopt QUESTION RIGHT_SHIFT"); }  //$NON-NLS-1$
		    consumeWildcard2(); 			break;
 
    case 794 : if (DEBUG) { System.out.println("Wildcard2 ::= TypeAnnotationsopt QUESTION..."); }  //$NON-NLS-1$
		    consumeWildcard2WithBounds(); 			break;
 
    case 795 : if (DEBUG) { System.out.println("WildcardBounds2 ::= extends ReferenceType2"); }  //$NON-NLS-1$
		    consumeWildcardBounds2Extends(); 			break;
 
    case 796 : if (DEBUG) { System.out.println("WildcardBounds2 ::= super ReferenceType2"); }  //$NON-NLS-1$
		    consumeWildcardBounds2Super(); 			break;
 
    case 797 : if (DEBUG) { System.out.println("Wildcard3 ::= TypeAnnotationsopt QUESTION..."); }  //$NON-NLS-1$
		    consumeWildcard3(); 			break;
 
    case 798 : if (DEBUG) { System.out.println("Wildcard3 ::= TypeAnnotationsopt QUESTION..."); }  //$NON-NLS-1$
		    consumeWildcard3WithBounds(); 			break;
 
    case 799 : if (DEBUG) { System.out.println("WildcardBounds3 ::= extends ReferenceType3"); }  //$NON-NLS-1$
		    consumeWildcardBounds3Extends(); 			break;
 
    case 800 : if (DEBUG) { System.out.println("WildcardBounds3 ::= super ReferenceType3"); }  //$NON-NLS-1$
		    consumeWildcardBounds3Super(); 			break;
 
    case 801 : if (DEBUG) { System.out.println("TypeParameterHeader ::= TypeAnnotationsopt Identifier"); }  //$NON-NLS-1$
		    consumeTypeParameterHeader(); 			break;
 
    case 802 : if (DEBUG) { System.out.println("TypeParameters ::= LESS TypeParameterList1"); }  //$NON-NLS-1$
		    consumeTypeParameters(); 			break;
 
    case 804 : if (DEBUG) { System.out.println("TypeParameterList ::= TypeParameterList COMMA..."); }  //$NON-NLS-1$
		    consumeTypeParameterList(); 			break;
 
    case 806 : if (DEBUG) { System.out.println("TypeParameter ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
		    consumeTypeParameterWithExtends(); 			break;
 
    case 807 : if (DEBUG) { System.out.println("TypeParameter ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
		    consumeTypeParameterWithExtendsAndBounds(); 			break;
 
    case 809 : if (DEBUG) { System.out.println("AdditionalBoundList ::= AdditionalBoundList..."); }  //$NON-NLS-1$
		    consumeAdditionalBoundList(); 			break;
 
    case 810 : if (DEBUG) { System.out.println("AdditionalBound ::= AND ReferenceType"); }  //$NON-NLS-1$
		    consumeAdditionalBound(); 			break;
 
    case 812 : if (DEBUG) { System.out.println("TypeParameterList1 ::= TypeParameterList COMMA..."); }  //$NON-NLS-1$
		    consumeTypeParameterList1(); 			break;
 
    case 813 : if (DEBUG) { System.out.println("TypeParameter1 ::= TypeParameterHeader GREATER"); }  //$NON-NLS-1$
		    consumeTypeParameter1(); 			break;
 
    case 814 : if (DEBUG) { System.out.println("TypeParameter1 ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
		    consumeTypeParameter1WithExtends(); 			break;
 
    case 815 : if (DEBUG) { System.out.println("TypeParameter1 ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
		    consumeTypeParameter1WithExtendsAndBounds(); 			break;
 
    case 817 : if (DEBUG) { System.out.println("AdditionalBoundList1 ::= AdditionalBoundList..."); }  //$NON-NLS-1$
		    consumeAdditionalBoundList1(); 			break;
 
    case 818 : if (DEBUG) { System.out.println("AdditionalBound1 ::= AND ReferenceType1"); }  //$NON-NLS-1$
		    consumeAdditionalBound1(); 			break;
 
    case 824 : if (DEBUG) { System.out.println("UnaryExpression_NotName ::= PLUS PushPosition..."); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.PLUS); 			break;
 
    case 825 : if (DEBUG) { System.out.println("UnaryExpression_NotName ::= MINUS PushPosition..."); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.MINUS); 			break;
 
    case 828 : if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus_NotName ::= TWIDDLE..."); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.TWIDDLE); 			break;
 
    case 829 : if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus_NotName ::= NOT PushPosition"); }  //$NON-NLS-1$
		    consumeUnaryExpression(OperatorIds.NOT); 			break;
 
    case 832 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.MULTIPLY); 			break;
 
    case 833 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::= Name MULTIPLY..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.MULTIPLY); 			break;
 
    case 834 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.DIVIDE); 			break;
 
    case 835 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::= Name DIVIDE..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.DIVIDE); 			break;
 
    case 836 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.REMAINDER); 			break;
 
    case 837 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::= Name REMAINDER..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.REMAINDER); 			break;
 
    case 839 : if (DEBUG) { System.out.println("AdditiveExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.PLUS); 			break;
 
    case 840 : if (DEBUG) { System.out.println("AdditiveExpression_NotName ::= Name PLUS..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.PLUS); 			break;
 
    case 841 : if (DEBUG) { System.out.println("AdditiveExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.MINUS); 			break;
 
    case 842 : if (DEBUG) { System.out.println("AdditiveExpression_NotName ::= Name MINUS..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.MINUS); 			break;
 
    case 844 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= ShiftExpression_NotName..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.LEFT_SHIFT); 			break;
 
    case 845 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= Name LEFT_SHIFT..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.LEFT_SHIFT); 			break;
 
    case 846 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= ShiftExpression_NotName..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.RIGHT_SHIFT); 			break;
 
    case 847 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= Name RIGHT_SHIFT..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.RIGHT_SHIFT); 			break;
 
    case 848 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= ShiftExpression_NotName..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.UNSIGNED_RIGHT_SHIFT); 			break;
 
    case 849 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= Name UNSIGNED_RIGHT_SHIFT..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.UNSIGNED_RIGHT_SHIFT); 			break;
 
    case 851 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= ShiftExpression_NotName"); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.LESS); 			break;
 
    case 852 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= Name LESS..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.LESS); 			break;
 
    case 853 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= ShiftExpression_NotName"); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.GREATER); 			break;
 
    case 854 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= Name GREATER..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.GREATER); 			break;
 
    case 855 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.LESS_EQUAL); 			break;
 
    case 856 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= Name LESS_EQUAL..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.LESS_EQUAL); 			break;
 
    case 857 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.GREATER_EQUAL); 			break;
 
    case 858 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= Name GREATER_EQUAL..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.GREATER_EQUAL); 			break;
 
    case 860 : if (DEBUG) { System.out.println("InstanceofExpression_NotName ::= Name InstanceofRHS"); }  //$NON-NLS-1$
		    consumeInstanceOfExpressionWithName(); 			break;
 
    case 861 : if (DEBUG) { System.out.println("InstanceofExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeInstanceOfExpression(); 			break;
 
    case 863 : if (DEBUG) { System.out.println("EqualityExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeEqualityExpression(OperatorIds.EQUAL_EQUAL); 			break;
 
    case 864 : if (DEBUG) { System.out.println("EqualityExpression_NotName ::= Name EQUAL_EQUAL..."); }  //$NON-NLS-1$
		    consumeEqualityExpressionWithName(OperatorIds.EQUAL_EQUAL); 			break;
 
    case 865 : if (DEBUG) { System.out.println("EqualityExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeEqualityExpression(OperatorIds.NOT_EQUAL); 			break;
 
    case 866 : if (DEBUG) { System.out.println("EqualityExpression_NotName ::= Name NOT_EQUAL..."); }  //$NON-NLS-1$
		    consumeEqualityExpressionWithName(OperatorIds.NOT_EQUAL); 			break;
 
    case 868 : if (DEBUG) { System.out.println("AndExpression_NotName ::= AndExpression_NotName AND..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.AND); 			break;
 
    case 869 : if (DEBUG) { System.out.println("AndExpression_NotName ::= Name AND EqualityExpression"); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.AND); 			break;
 
    case 871 : if (DEBUG) { System.out.println("ExclusiveOrExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.XOR); 			break;
 
    case 872 : if (DEBUG) { System.out.println("ExclusiveOrExpression_NotName ::= Name XOR AndExpression"); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.XOR); 			break;
 
    case 874 : if (DEBUG) { System.out.println("InclusiveOrExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.OR); 			break;
 
    case 875 : if (DEBUG) { System.out.println("InclusiveOrExpression_NotName ::= Name OR..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.OR); 			break;
 
    case 877 : if (DEBUG) { System.out.println("ConditionalAndExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.AND_AND); 			break;
 
    case 878 : if (DEBUG) { System.out.println("ConditionalAndExpression_NotName ::= Name AND_AND..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.AND_AND); 			break;
 
    case 880 : if (DEBUG) { System.out.println("ConditionalOrExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeBinaryExpression(OperatorIds.OR_OR); 			break;
 
    case 881 : if (DEBUG) { System.out.println("ConditionalOrExpression_NotName ::= Name OR_OR..."); }  //$NON-NLS-1$
		    consumeBinaryExpressionWithName(OperatorIds.OR_OR); 			break;
 
    case 883 : if (DEBUG) { System.out.println("ConditionalExpression_NotName ::=..."); }  //$NON-NLS-1$
		    consumeConditionalExpression(OperatorIds.QUESTIONCOLON) ; 			break;
 
    case 884 : if (DEBUG) { System.out.println("ConditionalExpression_NotName ::= Name QUESTION..."); }  //$NON-NLS-1$
		    consumeConditionalExpressionWithName(OperatorIds.QUESTIONCOLON) ; 			break;
 
    case 888 : if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= Modifiers AT..."); }  //$NON-NLS-1$
		    consumeAnnotationTypeDeclarationHeaderName() ; 			break;
 
    case 889 : if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= Modifiers AT..."); }  //$NON-NLS-1$
		    consumeAnnotationTypeDeclarationHeaderNameWithTypeParameters() ; 			break;
 
    case 890 : if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= AT..."); }  //$NON-NLS-1$
		    consumeAnnotationTypeDeclarationHeaderNameWithTypeParameters() ; 			break;
 
    case 891 : if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= AT..."); }  //$NON-NLS-1$
		    consumeAnnotationTypeDeclarationHeaderName() ; 			break;
 
    case 892 : if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeader ::=..."); }  //$NON-NLS-1$
		    consumeAnnotationTypeDeclarationHeader() ; 			break;
 
    case 893 : if (DEBUG) { System.out.println("AnnotationTypeDeclaration ::=..."); }  //$NON-NLS-1$
		    consumeAnnotationTypeDeclaration() ; 			break;
 
    case 895 : if (DEBUG) { System.out.println("AnnotationTypeMemberDeclarationsopt ::="); }  //$NON-NLS-1$
		    consumeEmptyAnnotationTypeMemberDeclarationsopt() ; 			break;
 
    case 896 : if (DEBUG) { System.out.println("AnnotationTypeMemberDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
		    consumeAnnotationTypeMemberDeclarationsopt() ; 			break;
 
    case 898 : if (DEBUG) { System.out.println("AnnotationTypeMemberDeclarations ::=..."); }  //$NON-NLS-1$
		    consumeAnnotationTypeMemberDeclarations() ; 			break;
 
    case 899 : if (DEBUG) { System.out.println("AnnotationMethodHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
		    consumeMethodHeaderNameWithTypeParameters(true); 			break;
 
    case 900 : if (DEBUG) { System.out.println("AnnotationMethodHeaderName ::= Modifiersopt Type..."); }  //$NON-NLS-1$
		    consumeMethodHeaderName(true); 			break;
 
    case 901 : if (DEBUG) { System.out.println("AnnotationMethodHeaderDefaultValueopt ::="); }  //$NON-NLS-1$
		    consumeEmptyMethodHeaderDefaultValue() ; 			break;
 
    case 902 : if (DEBUG) { System.out.println("AnnotationMethodHeaderDefaultValueopt ::= DefaultValue"); }  //$NON-NLS-1$
		    consumeMethodHeaderDefaultValue(); 			break;
 
    case 903 : if (DEBUG) { System.out.println("AnnotationMethodHeader ::= AnnotationMethodHeaderName..."); }  //$NON-NLS-1$
		    consumeMethodHeader(); 			break;
 
    case 904 : if (DEBUG) { System.out.println("AnnotationTypeMemberDeclaration ::=..."); }  //$NON-NLS-1$
		    consumeAnnotationTypeMemberDeclaration() ; 			break;
 
    case 912 : if (DEBUG) { System.out.println("AnnotationName ::= AT UnannotatableName"); }  //$NON-NLS-1$
		    consumeAnnotationName() ; 			break;
 
    case 913 : if (DEBUG) { System.out.println("NormalAnnotation ::= AnnotationName LPAREN..."); }  //$NON-NLS-1$
		    consumeNormalAnnotation(false) ; 			break;
 
    case 914 : if (DEBUG) { System.out.println("MemberValuePairsopt ::="); }  //$NON-NLS-1$
		    consumeEmptyMemberValuePairsopt() ; 			break;
 
    case 917 : if (DEBUG) { System.out.println("MemberValuePairs ::= MemberValuePairs COMMA..."); }  //$NON-NLS-1$
		    consumeMemberValuePairs() ; 			break;
 
    case 918 : if (DEBUG) { System.out.println("MemberValuePair ::= SimpleName EQUAL EnterMemberValue..."); }  //$NON-NLS-1$
		    consumeMemberValuePair() ; 			break;
 
    case 919 : if (DEBUG) { System.out.println("EnterMemberValue ::="); }  //$NON-NLS-1$
		    consumeEnterMemberValue() ; 			break;
 
    case 920 : if (DEBUG) { System.out.println("ExitMemberValue ::="); }  //$NON-NLS-1$
		    consumeExitMemberValue() ; 			break;
 
    case 922 : if (DEBUG) { System.out.println("MemberValue ::= Name"); }  //$NON-NLS-1$
		    consumeMemberValueAsName() ; 			break;
 
    case 925 : if (DEBUG) { System.out.println("MemberValueArrayInitializer ::=..."); }  //$NON-NLS-1$
		    consumeMemberValueArrayInitializer() ; 			break;
 
    case 926 : if (DEBUG) { System.out.println("MemberValueArrayInitializer ::=..."); }  //$NON-NLS-1$
		    consumeMemberValueArrayInitializer() ; 			break;
 
    case 927 : if (DEBUG) { System.out.println("MemberValueArrayInitializer ::=..."); }  //$NON-NLS-1$
		    consumeEmptyMemberValueArrayInitializer() ; 			break;
 
    case 928 : if (DEBUG) { System.out.println("MemberValueArrayInitializer ::=..."); }  //$NON-NLS-1$
		    consumeEmptyMemberValueArrayInitializer() ; 			break;
 
    case 929 : if (DEBUG) { System.out.println("EnterMemberValueArrayInitializer ::="); }  //$NON-NLS-1$
		    consumeEnterMemberValueArrayInitializer() ; 			break;
 
    case 931 : if (DEBUG) { System.out.println("MemberValues ::= MemberValues COMMA MemberValue"); }  //$NON-NLS-1$
		    consumeMemberValues() ; 			break;
 
    case 932 : if (DEBUG) { System.out.println("MarkerAnnotation ::= AnnotationName"); }  //$NON-NLS-1$
		    consumeMarkerAnnotation(false) ; 			break;
 
    case 933 : if (DEBUG) { System.out.println("SingleMemberAnnotationMemberValue ::= MemberValue"); }  //$NON-NLS-1$
		    consumeSingleMemberAnnotationMemberValue() ; 			break;
 
    case 934 : if (DEBUG) { System.out.println("SingleMemberAnnotation ::= AnnotationName LPAREN..."); }  //$NON-NLS-1$
		    consumeSingleMemberAnnotation(false) ; 			break;
 
    case 935 : if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= Modifiersopt TypeParameters"); }  //$NON-NLS-1$
		    consumeRecoveryMethodHeaderNameWithTypeParameters(); 			break;
 
    case 936 : if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= Modifiersopt Type..."); }  //$NON-NLS-1$
		    consumeRecoveryMethodHeaderName(); 			break;
 
    case 937 : if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= ModifiersWithDefault..."); }  //$NON-NLS-1$
		    consumeRecoveryMethodHeaderNameWithTypeParameters(); 			break;
 
    case 938 : if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= ModifiersWithDefault Type"); }  //$NON-NLS-1$
		    consumeRecoveryMethodHeaderName(); 			break;
 
    case 939 : if (DEBUG) { System.out.println("RecoveryMethodHeader ::= RecoveryMethodHeaderName..."); }  //$NON-NLS-1$
		    consumeMethodHeader(); 			break;
 
    case 940 : if (DEBUG) { System.out.println("RecoveryMethodHeader ::= RecoveryMethodHeaderName..."); }  //$NON-NLS-1$
		    consumeMethodHeader(); 			break;
 
	}
}
