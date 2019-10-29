use rstd::{result};
use rstd::vec::Vec;
use support::{Parameter,  traits::Currency};
use sr_primitives::traits::{SimpleArithmetic, Bounded, Member};

use crate::linked_item::{LinkedItem};

pub trait NFTCurrency<AccountId> {
	type TokenId: Parameter + Member + SimpleArithmetic + Bounded + Default + Copy + Into<u64>;

	type Currency: Currency<AccountId>;

	fn symbol() -> Vec<u8>;

	fn name() -> Vec<u8>;

	fn token_uri(token_id: Self::TokenId) -> Vec<u8>;

	fn owner_of(token_id: Self::TokenId) -> AccountId;

	fn balance_of(account: AccountId) -> Self::TokenId;
	
	fn get_approved(token_id: Self::TokenId) -> Option<AccountId>;

	fn is_approved_for_all(account_approved: (AccountId, AccountId)) -> bool;

	fn total_supply() -> Self::TokenId;


	fn owner_to_token(account_token: (AccountId, Option<Self::TokenId>)) -> Option<LinkedItem<Self::TokenId>>;


	fn approve(
		who: &AccountId, 
		to:  Option<AccountId>, 
		token_id: Self::TokenId
	) -> result::Result<(), &'static str>;

	fn set_approval_for_all(
		who: &AccountId, 
		to: AccountId, 
		approved: bool
	) -> result::Result<(), &'static str>;

	// transfer
	fn transfer_from(
		who: &AccountId, 
		from: AccountId, 
		to: AccountId, 
		token_id: Self::TokenId
	) -> result::Result<(), &'static str>;

	// safe transfer
	fn safe_transfer_from(
		who: &AccountId, 
		from: AccountId, 
		to: AccountId, 
		token_id: Self::TokenId
	) -> result::Result<(), &'static str>;
}
