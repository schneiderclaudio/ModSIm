# Relatório Técnico — Simulador MODSIM

**Projeto:** ModSIM v3.6.15 — Motor de Simulação de Plantas de Beneficiamento de Minérios
**Data:** Maio de 2026

---

## 1. Introdução

O MODSIM é um sistema de simulação amplamente utilizado para plantas de processamento
mineral. O simulador possui uma arquitetura aberta que permite a adição de novos modelos
para operações unitárias de beneficiamento de minérios sempre que necessário. Essa
flexibilidade constitui um dos pontos fortes do sistema: a maior parte dos modelos
atualmente em uso rotineiro não existia quando o simulador foi desenvolvido originalmente.
A pesquisa em operações de processamento mineral garante um suprimento contínuo de
novas ideias de modelagem, muitas das quais são incorporadas ao MODSIM para ampliar
suas capacidades de simulação, testar modelos em amplas faixas de condições operacionais
e avaliar o comportamento de unidades em combinação com outras operações.

O simulador é composto por dois componentes principais:

- **Motor de simulação (DLL Fortran)** — localizado em `Modsim/Modsimdl/`, compilado
  como `ModsimMain.dll`. Contém toda a lógica de cálculo dos modelos de operações
  unitárias.
- **Interface gráfica (VB6)** — localizada em `Modsim/Modsimvb/`, responsável pelo
  desenho do fluxograma e pelo controle de execução de simulações.

---

## 2. Kit de Desenvolvimento de Software (SDK) para Modelos de Usuário

O SDK do MODSIM oferece duas interfaces simples e intuitivas que simplificam a
codificação de novos modelos e sua introdução no simulador. O SDK é baseado no
Microsoft Developer Studio e facilita a criação de uma biblioteca de vínculo dinâmico
(DLL) contendo o código dos novos modelos. Os modelos podem ser distribuídos como
DLLs e compartilhados entre desenvolvedores.

O MODSIM utiliza uma interface uniforme para modelos de operações unitárias. Desde que
os modelos respeitem esse padrão, podem ser tão simples ou complexos quanto necessário
para descrever realisticamente o comportamento da unidade modelada — desde modelos
de regressão simples até modelos de estado e parâmetros distribuídos complexos.

### 2.1 Adicionando um Novo Modelo ao MODSIM

O desenvolvimento de um novo modelo requer dois passos básicos:

1. O modelo deve ser definido e implementado em código computacional usando Fortran
   ou C, e compilado em uma biblioteca de vínculo dinâmico.
2. O novo modelo deve ser notificado ao MODSIM e quaisquer parâmetros de unidade
   que serão configurados pela interface gráfica durante a simulação devem ser
   descritos.

### 2.2 Estrutura do Workspace UserModels

O workspace `UserModels.dsw` contém os seguintes arquivos, cada um com uma função
específica:

| Arquivo | Função |
|---|---|
| `Connector.f90` | Sub-rotina exportada pela DLL; faz a ligação entre o MODSIM e as sub-rotinas do modelo do usuário |
| `ConnectorToC.cpp` | Equivalente ao Connector para modelos escritos em C |
| `Globals.f90` | Módulo com variáveis globais originadas na interface gráfica do MODSIM (gerado automaticamente por `DIMINP.FOR`) |
| `ModelVariables.f90` | Módulo que transmite dados do MODSIM para o modelo e retorna resultados |
| `UserModels.f90` | Arquivo onde as sub-rotinas dos modelos do usuário devem ser colocadas |
| `Utilities.f90` | Arquivo com sub-rotinas utilitárias disponíveis a todos os modelos |
| `CModelVariables.h` | Arquivo de cabeçalho C com as variáveis do módulo ModelVariables |

### 2.3 Variáveis do Módulo GLOBALS

O módulo `GLOBALS` contém variáveis globais provenientes da interface gráfica do MODSIM.
Essas variáveis podem ser utilizadas nos modelos do usuário, mas seus valores não devem
ser alterados, com exceção de `UnitExitValue`:

- **MaxOutputStreams** — número máximo de correntes de saída permitidas no fluxograma (inteiro)
- **MaxInputStreams** — número máximo de correntes de entrada permitidas no fluxograma (inteiro)
- **MeshSizes** — limites das classes de tamanho utilizadas pelo MODSIM nos cálculos internos (vetor real)
- **UnitExitValue** — valor de saída da unidade (inteiro)
- **UnitDiagFile** — número da unidade lógica do arquivo de diagnóstico na DLL principal (inteiro)
- **UnitJobPath** — nome do caminho para o diretório corrente (cadeia de caracteres)

### 2.4 Variáveis do Módulo ModelVariables

#### Variáveis transmitidas do MODSIM para o modelo

- **Feed(I,J,K)** — array tridimensional com a vazão mássica de sólidos na classe de tamanho I, classe de teor J e S-classe K (kg/s)
- **NumSizeClasses** — número de classes de tamanho em uso na simulação corrente (inteiro)
- **NumGClasses** — número de classes de teor em uso na simulação corrente (inteiro)
- **NumSClasses** — número de S-classes em uso na simulação corrente (inteiro)
- **NumberOfMinerals** — número de minerais nos dados do sistema
- **TotalSolidsF** — vazão total de sólidos na alimentação da unidade, em kg/s (real)
- **FeedWater** — vazão total de água na alimentação da unidade, em kg/s (real)
- **RepSize** — vetor de tamanhos representativos utilizados internamente pelo MODSIM, em metros (vetor real)
- **Parameters** — vetor de parâmetros que definem a condição operacional da unidade (vetor real)
- **SolidSpGr** — vetor de densidades relativas do material sólido em cada classe de teor (vetor real)
- **Texture** — vetor de parâmetros que caracterizam a textura mineralógica do minério (vetor real)
- **MagnSusceptG** — vetor de susceptibilidades magnéticas para o material em cada G-classe (vetor real)
- **OtherPropG** — vetor de propriedades físicas arbitrárias associadas a cada G-classe (vetor real)
- **FltnRateConsts** — vetor de constantes de taxa de flotação, uma por S-classe (vetor real)
- **MagnSusceptS** — vetor de susceptibilidades magnéticas para o material em cada S-classe (vetor real)
- **OtherPropS** — vetor de propriedades físicas arbitrárias associadas a cada S-classe (vetor real)
- **CalVal** — vetor de valores caloríficos para o material em cada G-classe, em J/kg (vetor real)
- **TotalSulfur** — vetor do teor total de enxofre do material em cada G-classe, em percentagem mássica (vetor real)
- **PyriticSulf** — vetor do teor de enxofre pirítico do material em cada G-classe, em percentagem mássica (vetor real)
- **GradeM(J,M)** — fração mássica do mineral M no material da classe de teor J (array real)
- **GradeV(J,M)** — fração volumétrica do mineral M no material da classe de teor J (array real)
- **UnitNumber** — número da unidade no fluxograma
- **Reporting** — variável lógica definida como `.TRUE.` quando o modelo deve enviar informações para o arquivo de relatório

#### Variáveis calculadas pelo modelo e retornadas ao MODSIM

- **Tailing(I,J,K)** — array com a vazão mássica de sólidos na corrente de rejeito (kg/s); todo modelo deve produzir ao menos uma corrente de rejeito
- **Concentrate(I,J,K)** — array com a vazão mássica de sólidos na corrente de concentrado (kg/s)
- **Middling(I,J,K)** — array com a vazão mássica de sólidos na corrente de middlings (kg/s)
- **TotalSolidsT** — vazão total de sólidos na corrente de rejeito, kg/s (real)
- **TailingsWater** — vazão total de água na corrente de rejeito, kg/s (real)
- **TotalSolidsC** — vazão total de sólidos na corrente de concentrado, kg/s (real)
- **ConcentrateWater** — vazão total de água no concentrado, kg/s (real)
- **TotalSolidsM** — vazão total de sólidos na corrente de middlings, kg/s (real)
- **MiddlingWater** — vazão total de água na corrente de middlings, kg/s (real)
- **NumberOfMessages** — número de mensagens de diagnóstico enviadas para impressão (definido automaticamente)

### 2.5 Conectando Sub-rotinas de Modelos ao MODSIM

**Em Fortran:** adicionar as seguintes duas linhas ao arquivo `Connector.f90`:

```fortran
Case 'NNNN'
  Call NomeDaSubrotina
```

onde `NNNN` é o mnemônico de quatro letras que identifica o modelo no MODSIM.

**Em C:** inserir a seguinte linha em `ConnectorToC.cpp`:

```c
if(strcmp(cmodel,"NNNN")==0) NomeDaFuncao();
```

### 2.6 Construindo a DLL UserModels

No Developer Studio original: **Build > Build UserModels.dll**. Corrigir quaisquer erros de
compilação ou linkedição reportados.

No ambiente modernizado descrito na Seção 4, utilizar o script de build correspondente.

### 2.7 Escrevendo o Arquivo de Relatório

O arquivo de relatório permite que a sub-rotina do modelo reporte o estado do modelo
quando o cálculo de simulação do fluxograma converge. O código de geração do relatório
deve ser incluído na sub-rotina do modelo e executado quando a variável `Reporting` for
verdadeira. Em sub-rotinas Fortran, os dados são escritos para o arquivo com número de
unidade lógica `iounit`; em programas C, para o arquivo apontado por `fptrRF`. Antes de
escrever qualquer informação de relatório, deve-se chamar a sub-rotina `ReportHeader`.

### 2.8 Notificando o MODSIM sobre o Novo Modelo

Abrir o SDK de Modelos de Usuário do MODSIM. Na aba **Model parameters**:

1. Selecionar o tipo de unidade na lista suspensa
2. Especificar o mnemônico de quatro letras do modelo
3. Especificar o número de parâmetros utilizados pelo modelo
4. Digitar os rótulos de prompt a serem exibidos na interface gráfica durante a edição dos parâmetros
5. Especificar um valor padrão adequado para cada parâmetro
6. Especificar as conversões de unidades a exibir (opções: `NONE`, `SIZE`, `FLOW`, `DENSITY`)

Na aba **Help text**, adicionar o texto de ajuda para o modelo. Os parâmetros podem ser
inseridos automaticamente clicando no botão **Add parameters**.

### 2.9 Depuração de Sub-rotinas de Modelos

O procedimento recomendado para testar e depurar um novo modelo é configurar um
fluxograma simples no MODSIM contendo apenas uma única operação unitária descrita
pelo novo modelo. Erros podem ser rastreados escrevendo mensagens de diagnóstico no
arquivo de diagnóstico da DLL, chamando:

```fortran
Message = 'Texto da mensagem'
Call Diagnostic(Message)
```

As mensagens de diagnóstico aparecem no arquivo de diagnóstico da DLL, acessível pelo
menu **Run** do formulário principal do MODSIM. Em caso de falha catastrófica na
sub-rotina, as mensagens estarão disponíveis no arquivo de diagnóstico do modelo do
usuário, também acessível pelo menu **Run**.

---

## 3. Ambiente de Compilação

### 3.1 Compilação da DLL Principal (Fortran)

A DLL principal `ModsimMain.dll` é compilada a partir dos fontes em
`Modsim/Modsimdl/`. O sistema de build suporta dois compiladores:

| Compilador | Comando |
|---|---|
| gfortran (MinGW/MSYS2) | `make` |
| Intel IFX (oneAPI) | `make COMPILER=ifx` |

Configurações de build disponíveis: `release` (padrão) e `debug` (`make BUILD=debug`).

O projeto Visual Studio (`ModS/ModS.sln`) pode ser aberto no VS2022 com a extensão
Intel Fortran, com as configurações: `Debug|Win32`, `Debug|x64`, `Release|Win32`,
`Release|x64`.

### 3.2 Ordem de Compilação dos Fontes

A ordem de compilação é obrigatória — módulos devem ser compilados antes dos
arquivos que os consomem:

1. `GLOBALS.F90`
2. `ModelVariables.f90`
3. `SIMOPMOD.F90`
4. Demais fontes

### 3.3 Dependências Externas

- **UserModels** — biblioteca estática pré-construída (`UserModels/$(BUILD)/UserModels.lib`)
  que deve existir antes da linkedição da DLL principal.
- **IMSL** — biblioteca numérica comercial de 32 bits (distribuída com o Compaq Visual
  Fortran 6.6). **Não utilizável em builds x64.**

---

## 4. Modernizações Realizadas

Esta seção descreve as intervenções técnicas realizadas durante o trabalho de
modernização do MODSIM, com o objetivo de produzir uma DLL x64 funcional utilizando o
compilador Intel IFX (oneAPI 2025.3.2) no VS2022.

### 4.1 Migração para Build x64 com Intel IFX

O toolchain original era o Compaq Visual Fortran 6.6 (CVF 6.6), que produz binários x86
de 32 bits. O objetivo do trabalho foi produzir uma DLL de 64 bits (`Modsim.dll`) com o
Intel IFX.

**Problema identificado:** o IFX exige que dois scripts de inicialização de ambiente sejam
chamados em sequência antes da compilação:

```bat
vcvarsall.bat x64
compiler\env\vars.bat intel64
```

A ausência do segundo script causava falha silenciosa do compilador. O script de build
`build.bat` foi criado em `C:\Users\User\AppData\Local\Temp\opencode\` com a sequência
correta de inicialização.

### 4.2 Remoção da Diretiva `/warn:interfaces`

A flag `/warn:interfaces` causava erros de compilação em arquivos legados com
incompatibilidades de tipo implícito (herança do Fortran 77 sem `IMPLICIT NONE`). A
diretiva foi removida do makefile `ModsimMain_ifx.mak` em vez de corrigir cada
incompatibilidade nos arquivos legados.

### 4.3 Compilação e Linkedição da UserModels DLL

A biblioteca UserModels foi compilada separadamente. Foram necessárias as seguintes
correções de interoperabilidade Fortran/C++:

- **`Connector.f90`** — o alias de exportação foi corrigido para `'cmodelroutines'`.
- **`ConnectorToC.cpp`** — adicionada declaração `extern "C"` para garantir a convenção
  de chamada correta na interface C/Fortran.
- **`CModelVariables.h`** — adicionadas guardas `extern "C"` para compatibilidade com C++.

A UserModels DLL compilou e linkeditou com sucesso.

### 4.4 Remoção da Dependência do IMSL

A biblioteca IMSL, originalmente utilizada para rotinas numéricas especializadas, é uma
biblioteca estática de **32 bits** distribuída com o CVF 6.6 (`C:\Program Files\Microsoft
Visual Studio\DF98\IMSL\LIB\`). Por incompatibilidade de formato de objeto (`14C` x86
versus `8664` x64), ela **não pode ser linkada em um binário de 64 bits**.

Foram identificadas as seguintes chamadas a rotinas IMSL no código-fonte da DLL
principal:

| Rotina IMSL | Arquivo | Descrição |
|---|---|---|
| `BETAI(x, a, b)` | `BETAAMD.FOR` | Função beta incompleta regularizada (precisão simples) |
| `DBETAI(x, a, b)` | `BETAAMD.FOR` | Função beta incompleta regularizada (precisão dupla) |
| `ANORDF(x)` | `BETAAMD.FOR` | Função de distribuição acumulada normal padrão |

Todas as três rotinas foram **substituídas por implementações puras em Fortran**,
acrescentadas ao final do arquivo `BETAAMD.FOR`:

- **`BETAI` / `DBETAI`** — implementadas via algoritmo de fração continuada de Lentz
  (Numerical Recipes §6.4), com a relação de simetria `I_x(a,b) = 1 - I_{1-x}(b,a)` para
  estabilidade numérica. A sub-rotina auxiliar `BETAI_DP` opera inteiramente em precisão
  dupla; `BETAI` e `DBETAI` são invólucros de precisão simples e dupla respectivamente.
- **`ANORDF`** — implementada usando a função intrínseca `ERFC` do Fortran:
  `Phi(x) = 0.5 * erfc(-x / sqrt(2))`.

Com a remoção das chamadas IMSL restantes em `ModsimMain`, as flags de linkedição
`-limsl -lIMSLS_ERR -lIMSLMPISTUB` foram removidas do makefile.

### 4.5 Limpeza de Objetos x86 Residuais

Arquivos `.obj` e `.mod` compilados anteriormente para x86 causavam o erro do linkeditor
`LNK1136: arquivo inválido ou corrompido` ao tentar produzir um binário x64. O diretório
`ModsimMain\Debug\` foi limpo antes de recompilar com IFX.

### 4.6 Resultado Final da Build

Após todas as intervenções acima, a DLL foi produzida com sucesso:

```
Modsim\Modsimdl\Modsim.dll   5 043 200 bytes   31/05/2026
Exit code: 0
```

Nenhum erro de linkedição foi reportado. A DLL x64 está disponível para uso com a
interface gráfica VB6 ou para chamadas diretas de outros componentes.

### 4.7 Trabalho Futuro

O arquivo `ModsimCurveFit.f90` (DLL separada) ainda contém chamadas pesadas ao IMSL
que não foram substituídas neste ciclo de trabalho:

| Rotina IMSL | Substituto proposto |
|---|---|
| `UNLSF` / `BCLSF` | MINPACK (mínimos quadrados não-lineares) |
| `LINRG` | LAPACK (`DGESV`) |
| `EVCRG` | LAPACK (`DSYEV`) |
| `FIN` | Implementação standalone |
| `UMACH` / `ERSET` / `WRCRN` / `WRRRL` | Eliminar (diagnóstico apenas) |

A substituição dessas rotinas constitui a próxima etapa do trabalho de modernização e o último passo para a desvinculação do projeto com o antigo ambiente de desenvolvimento "Compaq Visual Fortran" e a biblioteca "IMSL", ambos impeditivos para contribuições de terceiros no projeto open-source.
Há ainda espaço para outras modernizações que visam incentivar ainda mais a contribuição de terceiros ao projeto e possibilitar melhorias funcionais:
1) Migração da interface gráfica de Visual Basic 6 para C# (.NET Framework);
2) Migração de todos arquivos Fortran para a última versão da linguagem;
3) Implementação de uma pipeline CI/CD para compilação e distribuição automática de novas versões da aplicação;

---

## 5. Conclusão

O trabalho realizado permitiu migrar com sucesso o motor de simulação MODSIM do
toolchain legado Compaq Visual Fortran 6.6 (x86) para o Intel IFX oneAPI 2025.3.2 (x64),
eliminando as dependências incompatíveis com builds de 64 bits e substituindo rotinas
numéricas proprietárias da biblioteca IMSL por implementações de código aberto em
Fortran puro. A DLL `Modsim.dll` (5 MB, x64) é gerada sem erros a partir do código-fonte
atual.
