# substrate-nft

# 项目介绍
substrate-nft是一个构建在Substrate上的区块链项目，它实现了ERC-721的标准，并进行一些扩展。

 substrate-nft的开发、构建、编译和运行基于Substrate 2.0标准。

与Substrate一样，substrate-nft是基于Rust的，因此遵循Rust语言规则和语法。

说明：此项目仍然正在进行迭代更新和安全审查，可能会存在一些未解决的问题，请暂时不要在生产环境中使用。

# 非同质化令牌
非同质化令牌NFT与ERC20型令牌不同，每个令牌都是独一无二的。在我们的项目中，每个令牌都有一个用于表示令牌的唯一ID。从这里开始，每个令牌都可以拥有自己的值，自己的元数据，或者在应用程序或价值系统中具有特定用途。

批准、转移或管理令牌的非所有者帐户也是不同的，并且必须在每个令牌的基础上使用NFT完成。 加密猫是最著名的例子，其中实现了非同质化令牌，每个令牌代表平台上的小猫。

# NFT模块介绍
此项目是基于Substrate的，所以整个项目的架构和逻辑不再介绍，下面只对runtime下面的文件和模块进行说明。

## runtime文件结构
``` text
runtime/
├── build.rs
├── Cargo.toml
└── src
    ├── lib.rs
    ├── linked_item.rs
    ├── nft.rs
    └── template.rs
1 directory, 6 files
```
### 文件简单说明
* build.rs：额外的编译命令。
* Cargo.toml：cargo编译运行相关。
* lib.rs：Substrate自动生成的lib文件，和链有关。
* linked_item.rs：NFT资产数据存储链表。
* nft.rs：NFT核心逻辑。
* template.rs：Substrate自动生成的模板文件，可忽略。  

## 核心模块说明
nft.rs是ERC-721实现的核心逻辑，主要包括下面几个模块：

* Trait：定义相关类型。项目用到的有如下几种：
``` rust
type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;

type TokenId: Parameter + Member + SimpleArithmetic + Bounded + Default + Copy + Into<u64>;

type Currency: Currency<Self::AccountId>;
```

* decl_event：定义事件。定义的几个事件如下：
``` rust
Approval(AccountId, AccountId, TokenId)

ApprovalForAll(AccountId, AccountId, bool)

Transfer(AccountId, AccountId, TokenId)
```

* decl_storage：定义存储数据。ERC-721 Token相关的数据存储如下：
```rust
// 代币符号，默认NFT，Bytes存储
pub Symbol get(symbol)  config(): Vec<u8>;

// 代币名称，默认Non-Fungible Token，Bytes存储
pub Name get(name)  config(): Vec<u8>;

// 代币元数据uri
pub TokenURI get(token_uri): map T::TokenId => Vec<u8>;

// TokenId到账户的Map，用于保存、查找和修改Token持有人
pub TokenToOwner get(owner_of): map T::TokenId => T::AccountId;

// 账户到余额的Map，用于保存和查询账户持有Token数量
pub OwnerCount get(balance_of): map T::AccountId => T::TokenId;

// Token到授权账户的Map，用于保存和查询对单个Token的授权
pub TokenToApproval get(get_approved): map T::TokenId => Option<T::AccountId>;

// 一个账户对另一个账户是否授权的Map，用于保存和查询对一个账户对另一个账户的授权
pub OwnerToOperator get(is_approved_for_all): map (T::AccountId, T::AccountId) => bool;

// Token总发行量
pub TotalSupply get(total_supply): T::TokenId;

// 持有人和持有Token到链表的Map，用于查询持有人下的所有Token，并支持O(1)复杂度的修改持有人
pub OwnerToToken get(owner_to_token): map (T::AccountId, Option<T::TokenId>) => Option<TokenLinkedItem<T>>;
```

* decl_module：包括了dispatchable method 可以外部调用的函数
``` rust
fn deposit_event() = default;

fn approve(origin, to: T::AccountId, token_id: T::TokenId) 

fn set_approval_for_all(origin, to: T::AccountId, approved: bool) 

// transfer
fn transfer_from(origin, from: T::AccountId, to: T::AccountId, token_id: T::TokenId) -> Result 

// safe transfer
fn safe_transfer_from(origin, from: T::AccountId, to: T::AccountId, token_id: T::TokenId) -> Result
```

* 内部私有的功能接口
``` rust
// impl<T: Trait> Module<T>
fn do_appove(sender: &T::AccountId, to: &T::AccountId, token_id: &T::TokenId) -> Result

fn do_appove_for_all(owner: &T::AccountId, to: &T::AccountId, approved: bool) 

fn do_transfer(from: &T::AccountId, to: &T::AccountId, token_id: T::TokenId) -> Result 

fn approval_clear(token_id: T::TokenId) -> Result 
```

* 测试模块

测试示例都写在这里，通过*cargo test*方式运行测试。

``` rust
// tests for this module
#[cfg(test)]
mod tests {
  // ...
}
```

# 编译
## 安装Rust
``` shell
curl https://sh.rustup.rs -sSf | sh
```

## 依赖工具包安装
``` shell
git clone https://github.com/SubstrateOrg/substrate-nft.git

cd substrate-nft

./scripts/init.sh
```
## 编译
``` shell
# debug
cargo build   

# release 
cargo build --release 
```
# 运行
## 单节点开发链
可以使用下面的命令启动一条开发测试链：

``` shell
cargo run -- --dev
```
运行节点时，通过设置环境变量：**RUST_LOG=debug RUST_BACKTRACE=1 cargo run -- --dev**，可以查看到更详细的运行日志信息。

## 多节点本地测试网
如果你希望能在本地看到多节点共识算法运行情况，可以尝试创建一个本地测试网，其中包含两个针对Alice和Bob的验证者节点，这两个验证者节点是创世纪链中赋予测试网的初始权限。

你可以为每个节点命名并公开它们，以便在[Polkadot UI](https://telemetry.polkadot.io/#/Local%20Testnet)上查看到。

需要准备两个打开的命令行终端窗口。

我们将首先在默认TCP端口30333上启动Alice的基础节点，并将其链数据本地存储在/tmp/alice中。节点的引导节点ID是**QmRpheLN4JWdAnY7HGJfWFNbfkQCb6tFf4vvA6hgjMZKrR**，它是根据我们在下面指定的--node-key值生成的：

``` shell
cargo run -- \
  --base-path /tmp/alice \
  --chain=local \
  --alice \
  --node-key 0000000000000000000000000000000000000000000000000000000000000001 \
  --telemetry-url ws://telemetry.polkadot.io:1024 \
  --validator
```
在另一个命令行终端窗口中，我们将在不同的TCP端口30334上启动Bob的基础节点，并将其链数据本地存储在/tmp/bob中。我们将为--bootnodes选项指定一个值，该值会将其节点连接到TCP端口30333上的Alice的bootnode ID：

``` shell
cargo run -- \
  --base-path /tmp/bob \
  --bootnodes /ip4/127.0.0.1/tcp/30333/p2p/QmRpheLN4JWdAnY7HGJfWFNbfkQCb6tFf4vvA6hgjMZKrR \
  --chain=local \
  --bob \
  --port 30334 \
  --telemetry-url ws://telemetry.polkadot.io:1024 \
  --validator
```
通过**cargo run -- --help**可以查看到更多命令操作。

# 其他相关
* substrate-nft：[https://github.com/SubstrateOrg/substrate-nft](https://github.com/SubstrateOrg/substrate-nft)
* Substrate-dev：[https://substrate.dev/](https://substrate.dev/)
* cartes：[https://crates.parity.io](https://links.jianshu.com/go?to=https%3A%2F%2Fcrates.parity.io)
* Substrate：[https://github.com/paritytech/substrate](https://github.com/paritytech/substrate)
* ERC-721：[https://github.com/0xcert/ethereum-erc721](https://github.com/0xcert/ethereum-erc721)



